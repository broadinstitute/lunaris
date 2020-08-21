package lunaris.streams

import lunaris.streams.utils.RecordStreamTypes.{Meta, Record, RecordSource}
import lunaris.streams.utils.StreamTagger.TaggedItem
import lunaris.streams.utils.{StreamTagger, TaggedRecordOrdering}
import org.broadinstitute.yootilz.core.snag.Snag

object RecordStreamZipperWithFallback {

  sealed trait SourceId
  object DriverSourceId extends SourceId
  object DataSourceId extends SourceId

  class MergedTaggedRecordProcessor(fallBack: Record => Either[Snag, Record]) {
    def processNext(taggedRecord: TaggedItem[Record, SourceId]): Seq[Record] = {
      ???
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
