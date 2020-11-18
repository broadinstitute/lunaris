package lunaris.streams

import lunaris.genomics.Locus
import lunaris.streams.utils.RecordStreamTypes.{Meta, Record, RecordSource}
import lunaris.streams.utils.RecordTaggedSortedMerger
import lunaris.streams.utils.RecordTaggedSortedMerger.{TaggedEndMarker, TaggedItem, TaggedRecord}
import org.broadinstitute.yootilz.core.snag.Snag

object RecordStreamJoiner {
  type Joiner = (Record, Record) => Either[Snag, Record]
  type SnagLogger = Snag => Unit

  case class SourceId(i: Int)

  class MergedTaggedRecordProcessor(nSources: Int)(joiner: Joiner)(snagLogger: SnagLogger) {

    case class GotLastOf(gotLastsOf: Seq[Boolean]) {
      def addEndOf(sourceId: SourceId): GotLastOf = {
        GotLastOf(gotLastsOf.updated(sourceId.i, true))
      }
    }

    object GotLastOf {
      def create(): GotLastOf = GotLastOf(Seq.fill(nSources)(false))
    }

    case class BufferForLocus(locus: Locus, ids: Seq[String], records: Seq[Map[String, Record]]) {
      def add(sourceId: SourceId, record: Record): BufferForLocus = {
        val idAdded = record.id
        val idsNew = if(ids.contains(idAdded)) ids else ids :+ idAdded
        val i = sourceId.i
        val recordsForSourceOld = records(i)
        val recordsForSourceNew = recordsForSourceOld + (idAdded -> record)
        val recordsNew = records.updated(i, recordsForSourceNew)
        copy(ids = idsNew, records = recordsNew)
      }
    }

    object BufferForLocus {
      def fromRecord(sourceId: SourceId, record: Record): BufferForLocus = {
        val i = sourceId.i
        val records = Seq.tabulate(nSources) { j =>
          if(i == j) {
            Map(record.id -> record)
          } else {
            Map.empty[String, Record]
          }
        }
        BufferForLocus(record.locus, Seq(record.id), records)
      }
    }

    case class Buffer(gotLastOf: GotLastOf, buffersByLocus: Seq[BufferForLocus]) {
      def add(taggedItem: TaggedItem[SourceId]): Buffer = {
        taggedItem match {
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

      def joinPastLocus(): (Buffer, Seq[Record]) = ???

      def joinCompleted(): (Buffer, Seq[Record]) = ???

      def join(): (Buffer, Seq[Record]) = {
        val (bufferMinusPastLocus, joinedFromPastLocus) = joinPastLocus()
        val (bufferNew, joinedCompleted) = bufferMinusPastLocus.joinCompleted()
        (bufferNew, joinedFromPastLocus ++ joinedCompleted)
      }

      def process(taggedItem: TaggedItem[SourceId]): (Buffer, Seq[Record]) = add(taggedItem).join()
    }

    object Buffer {
      def create(): Buffer = Buffer(GotLastOf.create(), Seq.empty)
    }

    var buffer: Buffer = Buffer.create()

    def processItem(taggedItem: TaggedItem[SourceId]): Seq[Record] = {
      val (bufferNew, recordsJoined) = buffer.process(taggedItem)
      buffer = bufferNew
      recordsJoined
    }
  }

  def join(meta: Meta, sources: Seq[RecordSource])(joiner: Joiner)(snagLogger: SnagLogger): RecordSource = {
    val sourcesById = sources.zipWithIndex.map {
      case (source, i) => (SourceId(i), source)
    }.toMap
    val mergedTaggedSource = RecordTaggedSortedMerger.merge(sourcesById, meta.chroms)
    val mergedTaggedRecordProcessor = new MergedTaggedRecordProcessor(sources.size)(joiner)(snagLogger)
    mergedTaggedSource.statefulMapConcat(() => mergedTaggedRecordProcessor.processItem).mapMaterializedValue(_ => meta)
  }

}
