package lunaris.streams

import lunaris.genomics.Locus
import lunaris.streams.utils.RecordStreamTypes.{Meta, Record, RecordSource}
import lunaris.streams.utils.StreamTagger.TaggedItem
import lunaris.streams.utils.{StreamTagger, TaggedRecordOrdering}
import org.broadinstitute.yootilz.core.snag.Snag

object RecordStreamJoinerWithFallback {

  type Joiner = (Record, Record) => Either[Snag, Record]
  type Fallback = Record => Either[Snag, Record]
  type SnagLogger = Snag => Unit

  sealed trait SourceId

  object DriverSourceId extends SourceId

  object DataSourceId extends SourceId

  class MergedTaggedRecordProcessor(joiner: Joiner)(fallback: Fallback)(snagLogger: SnagLogger) {
    var state: State = InitialState

    def processNext(taggedRecord: TaggedItem[Record, SourceId]): Seq[Record] = {
      val (stateNew, recordsCombined) = state.withTaggedRecord(taggedRecord)
      state = stateNew
      recordsCombined
    }

    sealed trait State {
      def withTaggedRecord(taggedRecord: TaggedItem[Record, SourceId]): (StateWithLocus, Seq[Record])
    }

    object State {

      case class RecordStack(driverRecords: Seq[Record], dataRecords: Seq[Record]) {
        def withRecord(record: Record, sourceId: SourceId): RecordStack.Result = {
          sourceId match {
            case DriverSourceId => withDriverRecord(record)
            case DataSourceId => withDataRecord(record)
          }
        }

        def drainDriverRecords(): RecordStack.Result = {
          val generatedRecordsBuilder = Seq.newBuilder[Record]
          for(driverRecord <- driverRecords) {
            fallback(driverRecord) match {
              case Left(snag) => snagLogger(snag)
              case Right(record) => generatedRecordsBuilder += record
            }
          }
          val generatedRecords = generatedRecordsBuilder.result()
          RecordStack.Result(RecordStack(Seq(), dataRecords), generatedRecords)
        }

        def joinRecordsOrLog(driverRecord: Record, dataRecord: Record): Seq[Record] = {
          joiner(driverRecord, dataRecord) match {
            case Left(snag) =>
              snagLogger(snag)
              Seq()
            case Right(joinedRecord) =>
              Seq(joinedRecord)
          }
        }

        def withDriverRecord(driverRecord: Record): RecordStack.Result = {
          val id = driverRecord.id
          dataRecords.find(_.id == id) match {
            case Some(dataRecord) =>
              val joinedRecordsSeq = joinRecordsOrLog(driverRecord, dataRecord)
              val dataRecordsNew = dataRecords.filterNot(_.id == id)
              val stackNew = RecordStack(driverRecords, dataRecordsNew)
              RecordStack.Result(stackNew, joinedRecordsSeq)
            case None =>
              val stackNew = RecordStack(driverRecords :+ driverRecord, dataRecords)
              RecordStack.Result(stackNew, Seq())
          }
        }

        def withDataRecord(dataRecord: Record): RecordStack.Result = {
          val id = dataRecord.id
          driverRecords.find(_.id == id) match {
            case Some(driverRecord) =>
              val joinedRecordsSeq = joinRecordsOrLog(driverRecord, dataRecord)
              val driverRecordsNew = driverRecords.filterNot(_.id == id)
              val stackNew = RecordStack(driverRecordsNew, dataRecords)
              RecordStack.Result(stackNew, joinedRecordsSeq)
            case None =>
              val stackNew = RecordStack(driverRecords, dataRecords :+ dataRecord)
              RecordStack.Result(stackNew, Seq())
          }
        }
      }

      object RecordStack {

        case class Result(stack: RecordStack, records: Seq[Record])

        def empty: RecordStack = RecordStack(Seq(), Seq())

        def single(record: Record, sourceId: SourceId): RecordStack = {
          sourceId match {
            case DriverSourceId => RecordStack(Seq(record), Seq())
            case DataSourceId => RecordStack(Seq(), Seq(record))
          }
        }
      }

      case class GotLastRecords(gotLastDriverRecord: Boolean, gotLastDataRecord: Boolean) {
        def gotLastRecord: Boolean = gotLastDriverRecord && gotLastDataRecord

        def withSource(sourceId: SourceId, gotLast: Boolean): GotLastRecords = {
          sourceId match {
            case DriverSourceId => copy(gotLastDriverRecord = gotLast)
            case DataSourceId => copy(gotLastDataRecord = gotLast)
          }
        }
      }

      object GotLastRecords {
        def apply(): GotLastRecords = GotLastRecords(gotLastDriverRecord = false, gotLastDataRecord = false)
      }

    }

    object InitialState extends State {
      override def withTaggedRecord(taggedRecord: TaggedItem[Record, SourceId]): (StateWithLocus, Seq[Record]) = {
        val record = taggedRecord.item
        val sourceId = taggedRecord.sourceId
        val records = State.RecordStack.single(record, sourceId)
        val gotLastRecords = State.GotLastRecords().withSource(sourceId, taggedRecord.isLast)
        (StateWithLocus(record.locus, records, gotLastRecords), Seq())
      }
    }

    case class StateWithLocus(locus: Locus,
                              records: State.RecordStack,
                              gotLastRecords: State.GotLastRecords
                             ) extends State {
      override def withTaggedRecord(taggedRecord: TaggedItem[Record, SourceId]): (StateWithLocus, Seq[Record]) = {
        val record = taggedRecord.item
        val sourceId = taggedRecord.sourceId
        val gotLastRecordsNew = gotLastRecords.withSource(sourceId, taggedRecord.isLast)
        if (record.locus == locus) {
          val State.RecordStack.Result(recordsUnmatched, recordsJoined) = records.withRecord(record, sourceId)
          if (gotLastRecordsNew.gotLastRecord) {
            val State.RecordStack.Result(recordsRemaining, generatedRecords) =
              recordsUnmatched.drainDriverRecords()
            val stateNew = StateWithLocus(locus, recordsRemaining, gotLastRecordsNew)
            (stateNew, recordsJoined ++ generatedRecords)
          } else {
            val stateNew = StateWithLocus(locus, recordsUnmatched, gotLastRecordsNew)
            (stateNew, recordsJoined)
          }
        } else {
          val State.RecordStack.Result(_, generatedRecords) = records.drainDriverRecords()
          val (stateNew, _) = InitialState.withTaggedRecord(taggedRecord)
          if(gotLastRecordsNew.gotLastDriverRecord) {
            val State.RecordStack.Result(recordStackNew2, generatedRecords2) = stateNew.records.drainDriverRecords()
            val stateNew2 = StateWithLocus(record.locus, recordStackNew2, gotLastRecordsNew)
            (stateNew2, generatedRecords ++ generatedRecords2)
          } else {
            (stateNew, generatedRecords)
          }
        }
      }
    }
  }

  def joinWithFallback(meta: Meta,
                       driverSource: RecordSource,
                       dataSource: RecordSource)(
                        joiner: (Record, Record) => Either[Snag, Record]
                      )(
                        fallBack: Record => Either[Snag, Record]
                      )(
                        snagLogger: SnagLogger
                      ): RecordSource = {
    val driverSourceTagged = StreamTagger.tagSource[Record, Meta, SourceId](driverSource, DriverSourceId)
    val dataSourceTagged = StreamTagger.tagSource[Record, Meta, SourceId](dataSource, DataSourceId)
    implicit val taggedRecordOrdering: TaggedRecordOrdering[SourceId] = TaggedRecordOrdering(meta.chroms)
    val mergedTaggedSource = driverSourceTagged.mergeSorted(dataSourceTagged)
    val mergedTaggedRecordProcessor = new MergedTaggedRecordProcessor(joiner)(fallBack)(snagLogger)
    mergedTaggedSource.statefulMapConcat(() => mergedTaggedRecordProcessor.processNext).mapMaterializedValue(_ => meta)
  }

}
