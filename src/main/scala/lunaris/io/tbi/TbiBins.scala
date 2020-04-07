package lunaris.io.tbi

import lunaris.genomics.Region
import org.broadinstitute.yootilz.core.snag.Snag

object TbiBins {
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

  def offsetAtLevel(level: Int): Int = (1 << (3 * level) - 1) / 7

  def binAsRegion(k: Int): Region = {
    val level = level(k)
    val size = sizeAtLevel(level)
    val offset = offsetAtLevel(level)
    val start = (k - offset) * size
    val end = (k - offset + 1) * size
    Region(start, end)
  }
}
