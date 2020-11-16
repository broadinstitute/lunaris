package lunaris.streams.utils

import akka.stream.scaladsl.Source
import lunaris.streams.utils.RecordStreamTypes.{Meta, Record, RecordSource}

object RecordTaggedSortedMerger {

  sealed trait TaggedItem[S] {
    def sourceId: S
  }

  case class TaggedRecord[S](record: Record, sourceId: S) extends TaggedItem[S]
  case class TaggedEndMarker[S](sourceId: S) extends TaggedItem[S]

  def merge[S](sources: Map[S, RecordSource], chromosomes: Seq[String]): Source[TaggedItem[S], Meta] = {
    val taggedMarkedSources: Map[S, Source[TaggedItem[S], Meta]] = sources.map {
      case (sourceId, source) =>
        val taggedSource = source.map(record => TaggedRecord(record, sourceId))
        val taggedMarkedSource = taggedSource.concat(Source.single(TaggedEndMarker(sourceId)))
        (sourceId, taggedMarkedSource)
    }
    ???
  }

}
