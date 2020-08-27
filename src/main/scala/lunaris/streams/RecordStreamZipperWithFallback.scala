package lunaris.streams

import lunaris.genomics.Locus
import lunaris.streams.RecordStreamZipperWithFallback.MergedTaggedRecordProcessor.State.RecordStack
import lunaris.streams.RecordStreamZipperWithFallback.{DataSourceId, DriverSourceId, SourceId}
import lunaris.streams.utils.RecordStreamTypes.{Meta, Record, RecordSource}
import lunaris.streams.utils.StreamTagger.TaggedItem
import lunaris.streams.utils.{StreamTagger, TaggedRecordOrdering}
import org.broadinstitute.yootilz.core.snag.Snag

object RecordStreamZipperWithFallback {

  sealed trait SourceId

  object DriverSourceId extends SourceId

  object DataSourceId extends SourceId

  class MergedTaggedRecordProcessor(fallBack: Record => Either[Snag, Record]) {
    var state: MergedTaggedRecordProcessor.State = MergedTaggedRecordProcessor.InitialState

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

        def joinMatching(joiner: (Record, Record) => Record): (RecordStack, Seq[Record]) = {
          ???
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

    }

    object InitialState extends State {
      override def withTaggedRecord(taggedRecord: TaggedItem[Record, SourceId]): (StateWithLocus, Seq[Record]) = {
        val record = taggedRecord.item
        val sourceId = taggedRecord.sourceId
        val records = RecordStack.single(record, sourceId)
        val (gotLastDriver, gotLastData) = sourceId match {
          case DriverSourceId => (taggedRecord.isLast, false)
          case DataSourceId => (false, taggedRecord.isLast)
        }
        (StateWithLocus(record.locus, records, gotLastDriver, gotLastData), Seq())
      }
    }

    case class StateWithLocus(locus: Locus,
                              records: RecordStack,
                              gotLastDriverRecord: Boolean,
                              gotLastDataRecord: Boolean) extends State {
      override def withTaggedRecord(taggedRecord: TaggedItem[Record, SourceId]): (StateWithLocus, Seq[Record]) = {
        val record = taggedRecord.item
        val sourceId = taggedRecord.sourceId
        if (record.locus == locus) {
          val recordsWithRecord = records.withRecord(record, sourceId)


          ???
        } else {
          ???
        }
      }
    }

  }

  def zipWithFallback(meta: Meta,
                      driverSource: RecordSource,
                      dataSource: RecordSource)(
                       fallBack: Record => Either[Snag, Record]
                     ): RecordSource = {
    val driverSourceTagged = StreamTagger.tagSource[Record, Meta, SourceId](driverSource, DriverSourceId)
    val dataSourceTagged = StreamTagger.tagSource[Record, Meta, SourceId](dataSource, DataSourceId)
    implicit val taggedRecordOrdering: TaggedRecordOrdering[SourceId] = TaggedRecordOrdering(meta.chroms)
    val mergedTaggedSource = driverSourceTagged.mergeSorted(dataSourceTagged)
    val mergedTaggedRecordProcessor = new MergedTaggedRecordProcessor(fallBack)
    mergedTaggedSource.statefulMapConcat(() => mergedTaggedRecordProcessor.processNext).mapMaterializedValue(_ => meta)
  }

}
