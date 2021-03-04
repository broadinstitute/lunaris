package lunaris.genomics

import lunaris.utils.SortedMergedBuffer

final class LociSet(private val limitsByChrom: Map[String, Array[Int]]) {
  def contains(chrom: String, pos: Int): Boolean = {
    limitsByChrom.get(chrom) match {
      case Some(limits) => regionsContainPos(limits, pos)
      case None => false
    }
  }

  def coversLocus(locus: Locus): Boolean = {
    limitsByChrom.get(locus.chrom) match {
      case Some(limits) => regionsCoverRegion(limits, locus.region)
      case None => false
    }
  }
  def overlapsLocus(locus: Locus): Boolean = {
    limitsByChrom.get(locus.chrom) match {
      case Some(limits) => regionsOverlapRegion(limits, locus.region)
      case None => false
    }
  }

  @inline private def findIPos(limits: Array[Int], pos: Int): Int = {
    var iMin: Int = 0
    var iMax: Int = limits.length
    while (iMin < iMax) {
      if (iMax - iMin > 1) {
        val iMid = (iMin + iMax) / 2
        val limit = limits(iMid)
        if (pos < limit) {
          iMax = iMid
        } else {
          iMin = iMid + 1
        }
      } else {
        val limit = limits(iMin)
        if (pos < limit) {
          iMax = iMin
        } else {
          iMin = iMax
        }
      }
    }
    iMin
  }

  @inline private def regionsContainPos(limits: Array[Int], pos: Int): Boolean = {
    val iPos = findIPos(limits, pos)
    iPos % 2 == 1
  }

  @inline private def regionsCoverRegion(limits: Array[Int], region: Region): Boolean = {
    val begin = region.begin
    val end = region.end
    if(end > begin + 1) {
      val iBegin = findIPos(limits, begin)
      val iEnd = findIPos(limits, end)
      (iBegin % 2 == 1) && (iBegin == iEnd)
    } else {
      regionsContainPos(limits, begin)
    }
  }

  @inline private def regionsOverlapRegion(limits: Array[Int], region: Region): Boolean = {
    val begin = region.begin
    val end = region.end
    if(end > begin + 1) {
      val iBegin = findIPos(limits, begin)
      val iEnd = findIPos(limits, end)
      (iBegin % 2 == 1) || (iBegin != iEnd)
    } else {
      regionsContainPos(limits, begin)
    }
  }

  def size: Int = limitsByChrom.values.map(_.length / 2).sum
}

object LociSet {
  def newBuilder: Builder = new Builder

  final class Builder {

    private var buffersByChrom: Map[String, SortedMergedBuffer[Region]] = Map.empty

    def +=(locus: Locus): Unit = {
      val buffer = buffersByChrom.get(locus.chrom) match {
        case Some(existingBuffer) => existingBuffer
        case None =>
          val newBuffer = SortedMergedBuffer[Region](Ordering.ordered[Region])(_.touches(_))(_.mergeWith(_))
          buffersByChrom += (locus.chrom -> newBuffer)
          newBuffer
      }
      buffer += locus.region
    }

    private def regionsToLimits(regions: Seq[Region]): Array[Int] = {
      val limits = new Array[Int](2 * regions.size)
      regions.zipWithIndex.foreach { case (region, i) =>
        limits(2 * i) = region.begin
        limits(2 * i + 1) = region.end
      }
      limits
    }

    def result(): LociSet = {
      val limitsByChrom = buffersByChrom.view.mapValues(buffer => regionsToLimits(buffer.resultOpt().get)).toMap
      new LociSet(limitsByChrom)
    }
  }

}
