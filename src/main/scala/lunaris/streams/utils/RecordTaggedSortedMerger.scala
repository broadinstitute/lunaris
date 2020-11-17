package lunaris.streams.utils

import akka.stream.scaladsl.Source
import lunaris.genomics.LocusOrdering
import lunaris.streams.utils.RecordStreamTypes.{Meta, Record, RecordSource}
import lunaris.utils.SeqBasedOrdering

object RecordTaggedSortedMerger {

  sealed trait TaggedItem[S] {
    def sourceId: S
  }

  case class TaggedRecord[S](record: Record, sourceId: S) extends TaggedItem[S]

  case class TaggedEndMarker[S](sourceId: S) extends TaggedItem[S]

  case class TaggedItemOrdering[S](chromosomes: Seq[String]) extends Ordering[TaggedItem[S]] {
    val chromosomeOrdering: SeqBasedOrdering[String] = SeqBasedOrdering(chromosomes)
    val locusOrdering: LocusOrdering = new LocusOrdering(chromosomeOrdering)

    override def compare(item1: TaggedItem[S], item2: TaggedItem[S]): Int = {
      (item1, item2) match {
        case (TaggedRecord(record1, _), TaggedRecord(record2, _)) =>
          locusOrdering.compare(record1.locus, record2.locus)
        case (TaggedRecord(_, _), TaggedEndMarker(_)) => -1
        case (TaggedEndMarker(_), TaggedRecord(_, _)) => 1
        case (TaggedEndMarker(_), TaggedEndMarker(_)) => 0
      }
    }
  }

  def merge[S](sources: Map[S, RecordSource], chromosomes: Seq[String]): Source[TaggedItem[S], Meta] = {
    val taggedMarkedSources: Iterable[Source[TaggedItem[S], Meta]] = sources.map {
      case (sourceId, source) =>
        val taggedSource = source.map(record => TaggedRecord(record, sourceId))
        val taggedMarkedSource = taggedSource.concat(Source.single(TaggedEndMarker(sourceId)))
        (sourceId, taggedMarkedSource)
    }.values
    implicit val taggedItemOrdering: TaggedItemOrdering[S] = TaggedItemOrdering[S](chromosomes)
    taggedMarkedSources.reduce(_.mergeSorted(_))
  }

}
