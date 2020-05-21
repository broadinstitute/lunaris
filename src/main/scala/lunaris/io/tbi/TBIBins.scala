package lunaris.io.tbi

import lunaris.genomics.Region
import org.broadinstitute.yootilz.core.snag.Snag
import scala.collection.BufferedIterator

object TBIBins {
  val kMin: Int = 0
  val kMax: Int = 37449

  def level(k: Int): Int = {
    if (k == 0) {
      0
    } else if (k <= 8) {
      1
    } else if (k <= 72) {
      2
    } else if (k <= 584) {
      3
    } else if (k <= 4680) {
      4
    } else {
      5
    }
  }

  def snagOrLevel(k: Int): Either[Snag, Int] = {
    if (k < kMin) {
      Left(Snag(s"Bin number cannot be less than $kMin, but is $k."))
    } else if (k > kMax) {
      Left(Snag(s"Bin number cannot be larger than $kMax, but is $k."))
    } else {
      Right(level(k))
    }
  }

  def sizeAtLevel(level: Int): Int = 1 << (29 - 3 * level)

  def offsetAtLevel(level: Int): Int = ((1 << (3 * level)) - 1) / 7

  def binAsRegion(k: Int): Region = {
    val lev = level(k)
    val size = sizeAtLevel(lev)
    val offset = offsetAtLevel(lev)
    val start = (k - offset) * size
    val end = (k - offset + 1) * size
    Region(start, end)
  }

  /*
    int reg2bin(int beg, int end) {
      --end;
      if (beg>>14 == end>>14) return ((1<<15)-1)/7 + (beg>>14);
      if (beg>>17 == end>>17) return ((1<<12)-1)/7 + (beg>>17);
      if (beg>>20 == end>>20) return  ((1<<9)-1)/7 + (beg>>20);
      if (beg>>23 == end>>23) return  ((1<<6)-1)/7 + (beg>>23);
      if (beg>>26 == end>>26) return  ((1<<3)-1)/7 + (beg>>26);
      return 0;
    }
  */

  def findBinForRegion(region: Region): Int = {
    val beg = region.begin
    val end = region.end - 1
    if (beg >> 14 == end >> 14) {
      ((1 << 15) - 1) / 7 + (beg >> 14)
    } else if (beg >> 17 == end >> 17) {
      ((1 << 12) - 1) / 7 + (beg >> 17)
    } else if (beg >> 20 == end >> 20) {
      ((1 << 9) - 1) / 7 + (beg >> 20)
    } else if (beg >> 23 == end >> 23) {
      ((1 << 6) - 1) / 7 + (beg >> 23)
    } else if (beg >> 26 == end >> 26) {
      ((1 << 3) - 1) / 7 + (beg >> 26)
    } else {
      0
    }
  }

  /*
    #define MAX_BIN (((1<<18)-1)/7)
    int reg2bins(int rbeg, int rend, uint16_t list[MAX_BIN])
    {
      int i = 0, k;
      --rend;
      list[i++] = 0;
      for (k =    1 + (rbeg>>26); k <=    1 + (rend>>26); ++k) list[i++] = k;
      for (k =    9 + (rbeg>>23); k <=    9 + (rend>>23); ++k) list[i++] = k;
      for (k =   73 + (rbeg>>20); k <=   73 + (rend>>20); ++k) list[i++] = k;
      for (k =  585 + (rbeg>>17); k <=  585 + (rend>>17); ++k) list[i++] = k;
      for (k = 4681 + (rbeg>>14); k <= 4681 + (rend>>14); ++k) list[i++] = k;
      return i; // #elements in list[]
    }
   */

  def binsOverlappingRegion(region: Region): Set[Int] = {
    val rbeg = region.begin
    val rend = region.end - 1
    (
      Seq(0) ++
        (1 + (rbeg >> 26) to 1 + (rend >> 26)) ++
        (9 + (rbeg >> 23) to 9 + (rend >> 23)) ++
        (73 + (rbeg >> 20) to 73 + (rend >> 20)) ++
        (585 + (rbeg >> 17) to 585 + (rend >> 17)) ++
        (4681 + (rbeg >> 14) to 4681 + (rend >> 14))
      ).toSet
  }

  def binsOverlappingRegions(regions: Seq[Region]): Map[Region, Set[Int]] =
    regions.map(region => (region, binsOverlappingRegion(region))).toMap
}
