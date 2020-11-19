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

      def joinPastLocus(): (Buffer, Seq[Record]) = {
        if(buffersByLocus.size > 1) {
          val bufferForPastLocus = buffersByLocus.head
          var recordsRemaining: Seq[Map[String, Record]] = bufferForPastLocus.records
          val joinedRecordsBuilder = Seq.newBuilder[Record]
          for(id <- bufferForPastLocus.ids) {
            var recordJoinedOpt: Option[Record] = None
            var snagOpt: Option[Snag] = None
            val recordsForSourceIter = bufferForPastLocus.records.iterator
            while(recordsForSourceIter.hasNext && snagOpt.isEmpty) {
              val recordsForSource = recordsForSourceIter.next()
              recordsForSource.get(id) match {
                case Some(recordForId) =>
                  recordJoinedOpt match {
                    case Some(recordJoined) =>
                      joiner(recordJoined, recordForId) match {
                        case Left(snag) => snagOpt = Some(snag)
                        case Right(joinedRecordNew) => recordJoinedOpt = Some(joinedRecordNew)
                      }
                    case None =>
                      recordJoinedOpt = Some(recordForId)
                  }
                case None => ()
              }
            }
            (recordJoinedOpt, snagOpt) match {
              case (_, Some(snag)) => snagLogger(snag)
              case (Some(recordJoined), _) => joinedRecordsBuilder += recordJoined
              case (_, _) => ()
            }
            recordsRemaining = recordsRemaining.map(_ - id)
          }
          val recordsJoined = joinedRecordsBuilder.result()
          val buffersByLocusNew = buffersByLocus.tail
          (copy(buffersByLocus = buffersByLocusNew), recordsJoined)
        } else {
          (this, Seq.empty)
        }
      }

      def joinCompleted(): (Buffer, Seq[Record]) = {
        buffersByLocus.headOption match {
          case Some(bufferForLocus) =>
            var idsRemaining: Seq[String] = bufferForLocus.ids
            var encounteredJoiningObstacle: Boolean = false
            val joinedRecordsBuilder = Seq.newBuilder[Record]
            var recordsRemaining: Seq[Map[String, Record]] = bufferForLocus.records
            while((!encounteredJoiningObstacle) && idsRemaining.nonEmpty) {
              val id = idsRemaining.head
              var snagOpt: Option[Snag] = None
              var joinedRecordOpt: Option[Record] = None
              var iSource: Int = 0
              while((!encounteredJoiningObstacle) && snagOpt.isEmpty && iSource < nSources) {
                recordsRemaining(iSource).get(id) match {
                  case Some(recordForSource) =>
                    joinedRecordOpt match {
                      case Some(joinedRecord) =>
                        joiner(joinedRecord, recordForSource) match {
                          case Left(snag) => snagOpt = Some(snag)
                          case Right(joinedRecordNew) => joinedRecordOpt = Some(joinedRecordNew)
                        }
                      case None =>
                        joinedRecordOpt = Some(recordForSource)
                    }
                  case None =>
                    if(!gotLastOf.gotLastsOf(iSource)) {
                      encounteredJoiningObstacle = true
                    }
                }
                iSource += 1
              }
              if(!encounteredJoiningObstacle) {
                idsRemaining = idsRemaining.tail
                recordsRemaining = recordsRemaining.map(_ - id)
                (joinedRecordOpt, snagOpt) match {
                  case (_, Some(snag)) => snagLogger(snag)
                  case (Some(joinedRecord), _) => joinedRecordsBuilder += joinedRecord
                  case _ => ()
                }
              }
            }
            val joinedRecords = joinedRecordsBuilder.result()
            val bufferForLocusNew = bufferForLocus.copy(ids = idsRemaining, records = recordsRemaining)
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

  def join(meta: Meta, sources: Seq[RecordSource])(joiner: Joiner)(snagLogger: SnagLogger): RecordSource = {
    val sourcesById = sources.zipWithIndex.map {
      case (source, i) => (SourceId(i), source)
    }.toMap
    val mergedTaggedSource = RecordTaggedSortedMerger.merge(sourcesById, meta.chroms)
    val mergedTaggedRecordProcessor = new MergedTaggedRecordProcessor(sources.size)(joiner)(snagLogger)
    mergedTaggedSource.statefulMapConcat(() => mergedTaggedRecordProcessor.processItem).mapMaterializedValue(_ => meta)
  }

}
