package lunaris.genomics

case class Region(start: Int, end: Int) {
  def overlaps(oRegion: Region): Boolean = start < oRegion.end && end > oRegion.start
}
