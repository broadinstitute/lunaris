package lunaris.genomics

case class Region(begin: Int, end: Int) extends Ordered[Region] {
  def asString: String = s"$begin-$end"

  override def toString: String = asString

  def overlaps(oRegion: Region): Boolean = begin < oRegion.end && end > oRegion.begin

  override def compare(that: Region): Int = {
    val beginComparison = begin - that.begin
    if(beginComparison != 0) {
      beginComparison
    } else {
      end - that.end
    }
  }
}
