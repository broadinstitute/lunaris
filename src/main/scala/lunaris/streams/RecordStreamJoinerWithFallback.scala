package lunaris.streams

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

  case class DataSourceId(i: Int) extends SourceId

  case class GotLastOf(gotLastOfDriver: Boolean, gotLastsOfData: Seq[Boolean])

  object GotLastOf {
    def createNew(nDataStreams: Int): GotLastOf =
      GotLastOf(gotLastOfDriver = false, Seq.fill(nDataStreams)(false))
  }

  trait Buffer {
    def process(taggedRecord: TaggedItem[Record, SourceId]): (Buffer, Seq[Record])
  }

  trait NonTerminalBuffer extends Buffer {
    def nDataStreams: Int
  }

  case class InitialBuffer(nDataStreams: Int) extends NonTerminalBuffer {
    override def process(taggedRecord: TaggedItem[Record, SourceId]): (Buffer, Seq[Record]) = ???
  }

  case class DefaultBuffer(nDataStreams: Int, gotLastOf: GotLastOf) extends NonTerminalBuffer {
    override def process(taggedRecord: TaggedItem[Record, SourceId]): (Buffer, Seq[Record]) = ???
  }

  object TerminalBuffer extends Buffer {
    override def process(taggedRecord: TaggedItem[Record, SourceId]): (Buffer, Seq[Record]) =
      (TerminalBuffer, Seq.empty)
  }

  class MergedTaggedRecordProcessor(nDataStreams: Int)(joiner: Joiner)(fallback: Fallback)(snagLogger: SnagLogger) {
    def processNext(taggedRecord: TaggedItem[Record, SourceId]): Seq[Record] = {
      ???
    }
  }


  def joinWithFallback(meta: Meta,
                       driverSource: RecordSource,
                       dataSources: Seq[RecordSource]
                      )(
                        joiner: (Record, Record) => Either[Snag, Record]
                      )(
                        fallBack: Record => Either[Snag, Record]
                      )(
                        snagLogger: SnagLogger
                      ): RecordSource = {
    val driverSourceTagged = StreamTagger.tagSource[Record, Meta, SourceId](driverSource, DriverSourceId)
    val dataSourcesTagged = dataSources.zipWithIndex.map {
      case (dataSource, i) =>
        StreamTagger.tagSource[Record, Meta, SourceId](dataSource, DataSourceId(i))
    }
    implicit val taggedRecordOrdering: TaggedRecordOrdering[SourceId] = TaggedRecordOrdering(meta.chroms)
    val mergedTaggedSource = dataSourcesTagged.foldLeft(driverSourceTagged)(_.mergeSorted(_))
    val mergedTaggedRecordProcessor = new MergedTaggedRecordProcessor(dataSources.size)(joiner)(fallBack)(snagLogger)
    mergedTaggedSource.statefulMapConcat(() => mergedTaggedRecordProcessor.processNext).mapMaterializedValue(_ => meta)
  }
}
