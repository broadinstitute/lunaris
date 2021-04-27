package lunaris.genomics

import org.broadinstitute.yootilz.core.snag.Snag

final case class Locus(chrom: String, region: Region) {

}

object Locus {
  def parse(string: String, allowNoEnd: Boolean = false): Either[Snag, Locus] = {
    val parts = string.split(":")
    if (parts.length != 2) {
      Left(Snag(s"Don't know how to parse $string as a locus."))
    } else {
      val chrom = parts(0)
      val regionString = parts(1)
      Region.parse(regionString, allowNoEnd).map(Locus(chrom, _))
    }
  }
}
