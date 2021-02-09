package lunaris.utils

import org.scalatest.funsuite.AnyFunSuite

class MousseTest extends AnyFunSuite {
  private val max: Double = Long.MaxValue.toDouble

  private def longToDouble(long: Long): Double = long.toDouble / max

  class Stats {
    var count: Long = 0
    var sum: Double = 0.0
    var sumOfSquares: Double = 0.0
    var pre: Double = 0.0
    var prePre: Double = 0.0
    var sumAuto2: Double = 0.0
    var sumAuto3: Double = 0.0

    def add(long: Long): Unit = {
      count += 1
      val x = longToDouble(long)
      sum += x
      sumOfSquares += x * x
      sumAuto2 += pre * x
      sumAuto3 += prePre * pre * x
      prePre = pre
      pre = x
    }
    def mean: Double = sum / count
    def variance: Double = sumOfSquares / count
    def autoCorr2: Double = {
      val vari = variance
      sumAuto2 / (vari*(count - 1))
    }
    def autoCorr3: Double = {
      val vari = variance
      sumAuto3 / (vari*vari*(count - 2))
    }
  }

  test("Get some longs") {
    val mousse = Mousse()
    val random = mousse.asRandom
    val stats = new Stats
    for (_ <- 1 to 10) {
      for(_ <- 1 to 500000) {
        stats.add(random.nextLong())
      }
      println(s"mean = ${stats.mean}, variance = ${stats.variance}, " +
        s"autocorr2 = ${stats.autoCorr2}, autocorr3 = ${stats.autoCorr3}")
    }
  }
}
