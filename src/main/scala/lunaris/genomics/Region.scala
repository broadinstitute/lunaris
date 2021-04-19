package lunaris.genomics

import lunaris.utils.NumberParser
import org.broadinstitute.yootilz.core.snag.Snag

case class Region(begin: Int, end: Int) extends Ordered[Region] {
  def asString: String = s"$begin-$end"

  override def toString: String = asString

  def overlaps(oRegion: Region): Boolean = begin < oRegion.end && end > oRegion.begin

  def touches(oRegion: Region): Boolean = begin <= oRegion.end && end >= oRegion.begin

  def mergeWith(oRegion: Region): Region = Region(Math.min(begin, oRegion.begin), Math.max(end, oRegion.end))

  override def compare(that: Region): Int = {
    val beginComparison = begin - that.begin
    if (beginComparison != 0) {
      beginComparison
    } else {
      end - that.end
    }
  }
}

object Region {
  def parse(string: String): Either[Snag, Region] = {
    val parts = string.split("-")
    if (parts.length != 2) {
      Left(Snag(s"Don't know how to parse $string as a region."))
    } else {
      for {
        begin <- NumberParser.parseInt(parts(0))
        end <- NumberParser.parseInt(parts(1))
      } yield Region(begin, end)
    }
  }
}
