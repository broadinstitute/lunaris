package lunaris.genomics

case class Region(begin: Int, end: Int) extends Ordered[Region] {
  def asString: String = s"$begin-$end"

  override def toString: String = asString

  def overlaps(oRegion: Region): Boolean = begin < oRegion.end && end > oRegion.begin

  def touches(oRegion: Region): Boolean = begin <= oRegion.end && end >= oRegion.begin

  def mergeWith(oRegion: Region): Region = Region(Math.min(begin, oRegion.begin), Math.max(end, oRegion.end))

  override def compare(that: Region): Int = {
    val beginComparison = begin - that.begin
    if(beginComparison != 0) {
      beginComparison
    } else {
      end - that.end
    }
  }
}
