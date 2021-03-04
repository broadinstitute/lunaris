package lunaris.genomics

import lunaris.utils.SortedMergedBuffer

final class LociSet(private val limitsByChrom: Map[String, Array[Int]]) {
  def contains(variant: Variant): Boolean = {
    limitsByChrom.get(variant.chrom) match {
      case Some(limits) => regionsContainPos(limits, variant.pos)
      case None => false
    }
  }

  @inline private def regionsContainPos(limits: Array[Int], pos: Int): Boolean = {
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
    iMin % 2 == 1
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
