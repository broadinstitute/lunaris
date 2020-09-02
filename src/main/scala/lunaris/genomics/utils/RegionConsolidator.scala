package lunaris.genomics.utils

import lunaris.genomics.{Locus, Region}

import scala.collection.mutable

object RegionConsolidator {

  val defaultNimGapSize: Int = 1000000

  def newBuilderForSorted(minGapSize: Int = defaultNimGapSize): ConsolidatedBuilderForSorted =
    new ConsolidatedBuilderForSorted(minGapSize)

  class ConsolidatedBuilderForSorted(val minGapSize: Int= defaultNimGapSize ) {
    private var regionsBuildersByChrom: Map[String, mutable.Builder[Region, Seq[Region]]] = Map()
    private var currentLocusOpt: Option[Locus] = None

    private def addConsolidatedLocus(locus: Locus): Unit = {
      val builderForChrom = regionsBuildersByChrom.get(locus.chrom) match {
        case Some(builder) => builder
        case None =>
          val builder = Seq.newBuilder[Region]
          regionsBuildersByChrom += (locus.chrom -> builder)
          builder
      }
      println(locus)
      builderForChrom += locus.region
    }

    def add(locus: Locus): Unit = {
      currentLocusOpt match {
        case None => currentLocusOpt = Some(locus)
        case Some(currentLocus) =>
          if(currentLocus.chrom == locus.chrom && locus.region.end - currentLocus.region.begin < minGapSize) {
            val consolidatedLocus = Locus(currentLocus.chrom,
              Region(currentLocus.region.begin, Math.max(currentLocus.region.end, locus.region.end)))
            currentLocusOpt = Some(consolidatedLocus)
          } else {
            addConsolidatedLocus(currentLocus)
            currentLocusOpt = Some(locus)
          }
      }
    }

    private lazy val result_ : Map[String, Seq[Region]] = {
      currentLocusOpt.foreach { currentLocus => addConsolidatedLocus(currentLocus)}
      currentLocusOpt = None
      regionsBuildersByChrom.view.mapValues(_.result()).toMap
    }

    def result(): Map[String, Seq[Region]] = result_
  }

  def consolidate(regions: Seq[Region]): Seq[Region] = consolidateSorted(regions.sorted)

  def consolidateSorted(regions: Seq[Region]): Seq[Region] = {
    if (regions.isEmpty) {
      Seq.empty
    } else {
      val iter = regions.iterator
      var currentRegion: Region = iter.next()
      val builder = Seq.newBuilder[Region]
      while (iter.hasNext) {
        val nextRegion = iter.next()
        if (nextRegion.begin > currentRegion.end) {
          builder += currentRegion
          currentRegion = nextRegion
        } else {
          currentRegion = currentRegion.copy(end = Math.max(currentRegion.end, nextRegion.end))
        }
      }
      builder += currentRegion
      builder.result()
    }
  }

  def consolidateMap[K](regions: Map[K, Seq[Region]]): Map[K, Seq[Region]] = regions.view.mapValues(consolidate).toMap
}
