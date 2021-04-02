package lunaris.genomics

final case class Variant(chrom: String, pos: Int, ref: String, alt: String) {
  def toLocus: Locus = Locus(chrom, Region(pos, pos + ref.length))

  def toCanonicalId: String = s"$chrom:$pos:$ref:$alt"
}

object Variant {
  val sequenceCanonicalizationMaxLength: Int = 5

  def canonicalizeIfShort(string: String): String = {
    if (string.length <= sequenceCanonicalizationMaxLength) {
      string.intern()
    } else {
      string
    }
  }

  def apply(chrom: String, pos: Int, ref: String, alt: String): Variant =
    new Variant(chrom.intern(), pos, canonicalizeIfShort(ref), canonicalizeIfShort(alt))
}
