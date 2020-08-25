package lunaris.streams

import lunaris.genomics.Locus
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
    object InitialState extends State {
      override def withTaggedRecord(taggedRecord: TaggedItem[Record, SourceId]): (StateWithLocus, Seq[Record]) = {
        val record = taggedRecord.item
        taggedRecord.sourceId match {
          case DriverSourceId =>
            (StateWithLocus(record.locus, Seq(record), Seq(), taggedRecord.isLast, gotLastDataRecord = false), Seq())
          case DataSourceId =>
            (StateWithLocus(record.locus, Seq(), Seq(record), gotLastDriverRecord = false, taggedRecord.isLast), Seq())
        }
      }
    }
    case class StateWithLocus(locus: Locus,
                              driverRecords: Seq[Record],
                              dataRecords: Seq[Record],
                              gotLastDriverRecord: Boolean,
                              gotLastDataRecord: Boolean) extends State {
      override def withTaggedRecord(taggedRecord: TaggedItem[Record, SourceId]): (StateWithLocus, Seq[Record]) = {
        
        if(taggedRecord.item.locus != locus) {
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
