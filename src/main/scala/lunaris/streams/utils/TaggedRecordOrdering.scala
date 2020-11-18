package lunaris.streams.utils

import lunaris.genomics.LocusOrdering
import lunaris.streams.utils.StreamTaggerOld.TaggedItemOld
import lunaris.streams.utils.RecordStreamTypes.Record
import lunaris.utils.SeqBasedOrdering

case class TaggedRecordOrdering[S](chromosomes: Seq[String]) extends Ordering[TaggedItemOld[Record, S]] {
  val chromosomeOrdering: SeqBasedOrdering[String] = SeqBasedOrdering(chromosomes)
  val locusOrdering: LocusOrdering = new LocusOrdering(chromosomeOrdering)

  override def compare(taggedRecord1: TaggedItemOld[Record, S], taggedRecord2: TaggedItemOld[Record, S]): Int = {
    locusOrdering.compare(taggedRecord1.item.locus, taggedRecord2.item.locus)
  }
}
