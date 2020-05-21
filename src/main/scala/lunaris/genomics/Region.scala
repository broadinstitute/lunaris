package lunaris.genomics

case class Region(begin: Int, end: Int) extends Ordered[Region] {
  def asString: String = s"$begin-$end"

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

object Region {
  object RegionOrdering extends Ordering[Region] {
    override def compare(region1: Region, region2: Region): Int = {
      val beginComparison = region1.begin - region2.begin
      if(beginComparison != 0) {
        beginComparison
      } else {
        region1.end - region2.end
      }
    }
  }
}
