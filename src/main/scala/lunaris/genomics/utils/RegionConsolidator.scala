package lunaris.genomics.utils

import lunaris.genomics.Region

object RegionConsolidator {
  def consolidate(regions: Seq[Region]): Seq[Region] = consolidateSorted(regions.sorted)

  def consolidateSorted(regions: Seq[Region]): Seq[Region] = {
    if(regions.isEmpty) {
      Seq.empty
    } else {
      val iter = regions.iterator
      var currentRegion: Region = iter.next()
      val builder = Seq.newBuilder[Region]
      while(iter.hasNext) {
        val nextRegion = iter.next()
        if(nextRegion.begin > currentRegion.end) {
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
