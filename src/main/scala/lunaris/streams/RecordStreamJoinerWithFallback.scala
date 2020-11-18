package lunaris.streams

import lunaris.genomics.Locus
import lunaris.streams.utils.RecordStreamTypes.{Meta, Record, RecordSource}
import lunaris.streams.utils.RecordTaggedSortedMerger
import lunaris.streams.utils.RecordTaggedSortedMerger.{TaggedEndMarker, TaggedItem, TaggedRecord}
import org.broadinstitute.yootilz.core.snag.Snag

object RecordStreamJoinerWithFallback {
  type Joiner = (Record, Record) => Either[Snag, Record]
  type Fallback = Record => Either[Snag, Record]
  type SnagLogger = Snag => Unit

  sealed trait SourceId

  case object DriverSourceId extends SourceId

  case class DataSourceId(i: Int) extends SourceId

  class MergedTaggedRecordProcessor(nDataSources: Int)(joiner: Joiner)(fallback: Fallback)(snagLogger: SnagLogger) {

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

    case class BufferForLocus(locus: Locus, drivers: Seq[Record], dataRecords: Seq[Map[String, Record]]) {
      def add(sourceId: SourceId, record: Record): BufferForLocus = {
        sourceId match {
          case DriverSourceId =>
            copy(drivers = drivers :+ record)
          case DataSourceId(i) =>
            val recordsForSourceOld = dataRecords(i)
            val recordsForSourceNew = recordsForSourceOld + (record.id -> record)
            copy(dataRecords = dataRecords.updated(i, recordsForSourceNew))
        }
      }
    }

    object BufferForLocus {
      def fromRecord(sourceId: SourceId, record: Record): BufferForLocus = {
        val locus = record.locus
        sourceId match {
          case DriverSourceId =>
            BufferForLocus(locus, Seq(record), Seq.fill(nDataSources)(Map.empty))
          case DataSourceId(i) =>
            val dataRecords = Seq.tabulate[Map[String, Record]](nDataSources) { j =>
              if(j == i) {
                Map(record.id -> record)
              } else {
                Map.empty
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

      def joinPastLocus(): (Buffer, Seq[Record]) = {
        if(buffersByLocus.size > 1) {
          val bufferForPastLocus = buffersByLocus.head
          val joinedRecordsBuilder = Seq.newBuilder[Record]
          var dataRecordsRemaining = bufferForPastLocus.dataRecords
          for(driverRecord <- bufferForPastLocus.drivers) {
            var snagOrJoinedRecord: Either[Snag, Record] = Right(driverRecord)
            val id = driverRecord.id
            var foundData: Boolean = false
            for(dataRecordsForSource <- dataRecordsRemaining) {
              dataRecordsForSource.get(id).foreach { dataRecord =>
                snagOrJoinedRecord = for {
                  joinedRecord <- snagOrJoinedRecord
                  joinedRecordNew <- joiner(joinedRecord, dataRecord)
                } yield joinedRecordNew
                foundData = true
              }
            }
            if(!foundData) {
              snagOrJoinedRecord = snagOrJoinedRecord.flatMap(fallback)
            }
            snagOrJoinedRecord match {
              case Left(snag) => snagLogger(snag)
              case Right(joinedRecord) => joinedRecordsBuilder += joinedRecord
            }
            dataRecordsRemaining = dataRecordsRemaining.map(_ - id)
          }
          val buffersByLocusNew = buffersByLocus.tail
          val joinedRecords = joinedRecordsBuilder.result()
          (copy(buffersByLocus = buffersByLocusNew), joinedRecords)
        } else {
          (this, Seq.empty)
        }
      }

      def joinCompleted(): (Buffer, Seq[Record]) = {
        buffersByLocus.headOption match {
          case Some(bufferForLocus) =>
            var driverRecordsRemaining: Seq[Record] = bufferForLocus.drivers
            var encounteredJoiningObstacle: Boolean = false
            val joinedRecordsBuilder = Seq.newBuilder[Record]
            var dataRecordsRemaining: Seq[Map[String, Record]] = bufferForLocus.dataRecords
            while((!encounteredJoiningObstacle) && driverRecordsRemaining.nonEmpty) {
              val driverRecord = driverRecordsRemaining.head
              val id = driverRecord.id
              var snagOrJoinedRecord: Either[Snag, Record] = Right(driverRecord)
              var iDataSource: Int = 0
              var foundData: Boolean = false
              while(snagOrJoinedRecord.isRight && (!encounteredJoiningObstacle) && iDataSource < nDataSources) {
                dataRecordsRemaining(iDataSource).get(id) match {
                  case Some(dataRecord) =>
                    snagOrJoinedRecord = for {
                      joinedRecord <- snagOrJoinedRecord
                      joinedRecordNew <- joiner(joinedRecord, dataRecord)
                    } yield joinedRecordNew
                    foundData = true
                  case None =>
                    if(!gotLastOf.gotLastsOfData(iDataSource)) {
                      encounteredJoiningObstacle = true
                    }
                }
                iDataSource += 1
              }
              if(!encounteredJoiningObstacle) {
                if(!foundData) {
                  snagOrJoinedRecord = snagOrJoinedRecord.flatMap(fallback)
                }
                driverRecordsRemaining = driverRecordsRemaining.tail
                dataRecordsRemaining = dataRecordsRemaining.map(_ - id)
                snagOrJoinedRecord match {
                  case Left(snag) => snagLogger(snag)
                  case Right(joinedRecord) => joinedRecordsBuilder += joinedRecord
                }
              }
            }
            val joinedRecords = joinedRecordsBuilder.result()
            val bufferForLocusNew =
              bufferForLocus.copy(drivers = driverRecordsRemaining, dataRecords = dataRecordsRemaining)
            val buffersByLocusNew = buffersByLocus.updated(0, bufferForLocusNew)
            val bufferNew = copy(buffersByLocus = buffersByLocusNew)
            (bufferNew, joinedRecords)
          case None =>
            (this, Seq.empty)
        }
      }

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


  def joinWithFallback(meta: Meta,
                       driverSource: RecordSource,
                       dataSources: Seq[RecordSource]
                      )(joiner: Joiner)(fallBack: Fallback)(snagLogger: SnagLogger): RecordSource = {
    val driverSourceById = Map[SourceId, RecordSource](DriverSourceId -> driverSource)
    val dataSourcesById = dataSources.zipWithIndex.map {
      case (dataSource, i) => (DataSourceId(i), dataSource)
    }.toMap
    val sourcesById: Map[SourceId, RecordSource] = driverSourceById ++ dataSourcesById
    val mergedTaggedSource = RecordTaggedSortedMerger.merge(sourcesById, meta.chroms)
    val mergedTaggedRecordProcessor = new MergedTaggedRecordProcessor(dataSources.size)(joiner)(fallBack)(snagLogger)
    mergedTaggedSource.statefulMapConcat(() => mergedTaggedRecordProcessor.processItem).mapMaterializedValue(_ => meta)
  }
}
