package lunaris.streams.utils

import lunaris.genomics.LocusOrdering
import lunaris.streams.utils.StreamTagger.TaggedItem
import lunaris.streams.utils.RecordStreamTypes.Record
import lunaris.utils.SeqBasedOrdering

case class TaggedRecordOrdering[S](chromosomes: Seq[String]) extends Ordering[TaggedItem[Record, S]] {
  val chromosomeOrdering: SeqBasedOrdering[String] = SeqBasedOrdering(chromosomes)
  val locusOrdering: LocusOrdering = new LocusOrdering(chromosomeOrdering)

  override def compare(taggedRecord1: TaggedItem[Record, S], taggedRecord2: TaggedItem[Record, S]): Int = {
    locusOrdering.compare(taggedRecord1.item.locus, taggedRecord2.item.locus)
  }
}
