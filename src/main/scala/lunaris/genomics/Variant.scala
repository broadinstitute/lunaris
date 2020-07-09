package lunaris.genomics

case class Variant(chrom: String, pos: Int, ref: String, alt: String) {
  def toLocus: Locus = Locus(chrom, Region(pos, pos + ref.length))
}
