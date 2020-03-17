package lunaris.utils

import scala.util.Random

case class AscendingLongIterator(n: Long, max: Long) extends Iterator[Long] {
  var remaining: Long = n
  var previous: Long = 0

  val random = new Random

  override def hasNext: Boolean = remaining > 0

  override def next(): Long = {
    val next =
      if (max - previous <= 1) {
        max
      } else if (remaining == 1) {
        previous + 1 + Random.nextLong(max - previous - 1)
      } else if (max - previous <= remaining) {
        previous + 1
      } else {
        previous + 1 + Random.nextLong(2 * (max - previous) / remaining - 1)
      }
    previous = next
    remaining -= 1
    next
  }
}
