package lunaris.streams.transform

import lunaris.genomics.Locus
import lunaris.recipes.values.RecordStreamWithMeta.Meta
import lunaris.streams.utils.RecordStreamTypes.{Record, RecordSource}
import lunaris.streams.utils.RecordTaggedSortedMerger
import lunaris.streams.utils.RecordTaggedSortedMerger.{TaggedEndMarker, TaggedItem, TaggedRecord}

object RecordStreamComplementFilter {

  sealed trait SourceId

  case object DriverSourceId extends SourceId

  case class DataSourceId(i: Int) extends SourceId

  class MergedTaggedRecordProcessor(nDataSources: Int) {

    case class GotLastOf(gotLastOfDriver: Boolean, gotLastsOfData: Seq[Boolean]) {
      def addEndOf(sourceId: SourceId): GotLastOf = {
        sourceId match {
          case DriverSourceId => copy(gotLastOfDriver = true)
          case DataSourceId(i) => copy(gotLastsOfData = gotLastsOfData.updated(i, true))
        }
      }
    }

    object GotLastOf {
      def create(): GotLastOf = GotLastOf(gotLastOfDriver = false, Seq.fill(nDataSources)(false))
    }

    case class BufferForLocus(locus: Locus, drivers: Seq[Record], dataRecords: Seq[Set[String]]) {
      def add(sourceId: SourceId, record: Record): BufferForLocus = {
        sourceId match {
          case DriverSourceId =>
            copy(drivers = drivers :+ record)
          case DataSourceId(i) =>
            val recordsForSourceOld = dataRecords(i)
            val recordsForSourceNew = recordsForSourceOld + record.id
            copy(dataRecords = dataRecords.updated(i, recordsForSourceNew))
        }
      }
    }

    object BufferForLocus {
      def fromRecord(sourceId: SourceId, record: Record): BufferForLocus = {
        val locus = record.locus
        sourceId match {
          case DriverSourceId =>
            BufferForLocus(locus, Seq(record), Seq.fill(nDataSources)(Set.empty))
          case DataSourceId(i) =>
            val dataRecords = Seq.tabulate[Set[String]](nDataSources) { j =>
              if (j == i) {
                Set(record.id)
              } else {
                Set.empty
              }
            }
            BufferForLocus(locus, Seq.empty, dataRecords)
        }
      }
    }

    case class Buffer(gotLastOf: GotLastOf, buffersByLocus: Seq[BufferForLocus]) {
      def add(taggedRecord: TaggedItem[SourceId]): Buffer = {
        taggedRecord match {
          case TaggedRecord(recordAdded, sourceId) =>
            val locusAdded = recordAdded.locus
            val buffersByLocusNew = buffersByLocus.lastOption match {
              case Some(bufferForLocusLast) if bufferForLocusLast.locus == locusAdded =>
                val bufferForLocusLastNew = bufferForLocusLast.add(sourceId, recordAdded)
                buffersByLocus.updated(buffersByLocus.size - 1, bufferForLocusLastNew)
              case _ =>
                buffersByLocus :+ BufferForLocus.fromRecord(sourceId, recordAdded)
            }
            copy(buffersByLocus = buffersByLocusNew)
          case TaggedEndMarker(sourceId) =>
            val gotLastOfNew = gotLastOf.addEndOf(sourceId)
            copy(gotLastOf = gotLastOfNew)
        }
      }

      def flushPastLocus(): (Buffer, Seq[Record]) = ???

      def flushCompleted(): (Buffer, Seq[Record]) = ???

      def join(): (Buffer, Seq[Record]) = {
        val (bufferMinusPastLocus, joinedFromPastLocus) = flushPastLocus()
        val (bufferNew, joinedCompleted) = bufferMinusPastLocus.flushCompleted()
        (bufferNew, joinedFromPastLocus ++ joinedCompleted)
      }

      def process(taggedItem: TaggedItem[SourceId]): (Buffer, Seq[Record]) = add(taggedItem).join()
    }

    object Buffer {
      def create(): Buffer = Buffer(GotLastOf.create(), Seq.empty)
    }

    var buffer: Buffer = Buffer.create()

    def processItem(taggedItem: TaggedItem[SourceId]): Seq[Record] = {
      val (bufferNew, recordsCleared) = buffer.process(taggedItem)
      buffer = bufferNew
      recordsCleared
    }
  }

  def diff(meta: Meta, driverSource: RecordSource, dataSources: Seq[RecordSource]): RecordSource = {
    val driverSourceById = Map[SourceId, RecordSource](DriverSourceId -> driverSource)
    val dataSourcesById = dataSources.zipWithIndex.map {
      case (dataSource, i) => (DataSourceId(i), dataSource)
    }.toMap
    val sourcesById: Map[SourceId, RecordSource] = driverSourceById ++ dataSourcesById
    val mergedTaggedSource = RecordTaggedSortedMerger.merge(sourcesById, meta.chroms)
    val mergedTaggedRecordProcessor = new MergedTaggedRecordProcessor(dataSources.size)
    mergedTaggedSource.statefulMapConcat(() => mergedTaggedRecordProcessor.processItem).mapMaterializedValue(_ => meta)
  }
}
