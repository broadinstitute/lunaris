package lunaris.streams

import lunaris.genomics.Locus
import lunaris.streams.RecordStreamZipperWithFallback.MergedTaggedRecordProcessor.State.{GotLastRecords, RecordStack}
import lunaris.streams.utils.RecordStreamTypes.{Meta, Record, RecordSource}
import lunaris.streams.utils.StreamTagger.TaggedItem
import lunaris.streams.utils.{StreamTagger, TaggedRecordOrdering}
import org.broadinstitute.yootilz.core.snag.Snag

object RecordStreamZipperWithFallback {

  type Joiner = (Record, Record) => Record
  type Fallback = Record => Either[Snag, Record]

  sealed trait SourceId

  object DriverSourceId extends SourceId

  object DataSourceId extends SourceId

  class MergedTaggedRecordProcessor(joiner: Joiner)(fallback: Fallback) {
    var state: MergedTaggedRecordProcessor.State = MergedTaggedRecordProcessor.InitialState(joiner)(fallback)

    def processNext(taggedRecord: TaggedItem[Record, SourceId]): Seq[Record] = {
      val (stateNew, recordsCombined) = state.withTaggedRecord(taggedRecord)
      state = stateNew
      recordsCombined
    }
  }

  object MergedTaggedRecordProcessor {

    sealed trait State {
      def withTaggedRecord(taggedRecord: TaggedItem[Record, SourceId]): (StateWithLocus, Seq[Record])
    }

    object State {

      case class RecordStack(driverRecords: Seq[Record], dataRecords: Seq[Record]) {
        def withRecord(record: Record, sourceId: SourceId): RecordStack = {
          sourceId match {
            case DriverSourceId => copy(driverRecords = driverRecords :+ record)
            case DataSourceId => copy(dataRecords = dataRecords :+ record)
          }
        }

        def joinMatching(joiner: Joiner): (RecordStack, Seq[Record]) = {
          val joinedRecordsBuilder = Seq.newBuilder[Record]
          var idsJoined: Set[String] = Set.empty
          for (driverRecord <- driverRecords) {
            val id = driverRecord.id
            dataRecords.find(_.id == id).foreach { dataRecord =>
              joinedRecordsBuilder += joiner(driverRecord, dataRecord)
              idsJoined += id
            }
          }
          val driverRecordsUnmatched = driverRecords.filterNot(record => idsJoined.contains(record.id))
          val dataRecordsUnmatched = dataRecords.filterNot(record => idsJoined.contains(record.id))
          (RecordStack(driverRecordsUnmatched, dataRecordsUnmatched), joinedRecordsBuilder.result())
        }
      }

      object RecordStack {
        def empty: RecordStack = RecordStack(Seq(), Seq())

        def single(record: Record, sourceId: SourceId): RecordStack = {
          sourceId match {
            case DriverSourceId => RecordStack(Seq(record), Seq())
            case DataSourceId => RecordStack(Seq(), Seq(record))
          }
        }
      }

      case class GotLastRecords(gotLastDriverRecord: Boolean, gotLastDataRecord: Boolean) {
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

    class InitialState(val joiner: Joiner)(val fallback: Fallback) extends State {
      override def withTaggedRecord(taggedRecord: TaggedItem[Record, SourceId]): (StateWithLocus, Seq[Record]) = {
        val record = taggedRecord.item
        val sourceId = taggedRecord.sourceId
        val records = RecordStack.single(record, sourceId)
        val gotLastRecords = GotLastRecords().withSource(sourceId, taggedRecord.isLast)
        (StateWithLocus(record.locus, records, gotLastRecords)(joiner)(fallback), Seq())
      }
    }

    object InitialState {
      def apply(joiner: Joiner)(fallback: Fallback): InitialState = new InitialState(joiner)(fallback)
    }

    case class StateWithLocus(locus: Locus,
                              records: RecordStack,
                             gotLastRecords: GotLastRecords,
                             )(joiner: Joiner)(fallback: Fallback) extends State {
      override def withTaggedRecord(taggedRecord: TaggedItem[Record, SourceId]): (StateWithLocus, Seq[Record]) = {
        val record = taggedRecord.item
        val sourceId = taggedRecord.sourceId
        val gotLastRecordsNew = gotLastRecords.withSource(sourceId, taggedRecord.isLast)
        if (record.locus == locus) {
          val (recordsNew, recordsJoined) =  records.withRecord(record, sourceId).joinMatching(joiner)
          if(gotLastRecordsNew.gotLastDriverRecord) {
            ???
          } else {
            (StateWithLocus(locus, recordsNew, gotLastRecordsNew)(joiner)(fallback), recordsJoined)
          }
        } else {
          ???
        }
      }
    }

  }

  def joinWithFallback(meta: Meta,
                       driverSource: RecordSource,
                       dataSource: RecordSource)(
                        joiner: (Record, Record) => Record
                      )(
                        fallBack: Record => Either[Snag, Record]
                      ): RecordSource = {
    val driverSourceTagged = StreamTagger.tagSource[Record, Meta, SourceId](driverSource, DriverSourceId)
    val dataSourceTagged = StreamTagger.tagSource[Record, Meta, SourceId](dataSource, DataSourceId)
    implicit val taggedRecordOrdering: TaggedRecordOrdering[SourceId] = TaggedRecordOrdering(meta.chroms)
    val mergedTaggedSource = driverSourceTagged.mergeSorted(dataSourceTagged)
    val mergedTaggedRecordProcessor = new MergedTaggedRecordProcessor(joiner)(fallBack)
    mergedTaggedSource.statefulMapConcat(() => mergedTaggedRecordProcessor.processNext).mapMaterializedValue(_ => meta)
  }

}
