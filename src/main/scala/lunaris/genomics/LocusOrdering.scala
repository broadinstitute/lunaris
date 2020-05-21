package lunaris.genomics

class LocusOrdering(chromOrdering: Ordering[String]) extends Ordering[Locus] {
  override def compare(locus1: Locus, locus2: Locus): Int = {
    val chromCompare = chromOrdering.compare(locus1.chrom, locus2.chrom)
    if(chromCompare != 0) {
      chromCompare
    } else {
      locus1.region.compare(locus2.region)
    }
  }
}
