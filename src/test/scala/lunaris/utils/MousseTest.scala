package lunaris.utils

import org.scalatest.funsuite.AnyFunSuite

import scala.util.Random

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

  private val epsilon: Double = 1e-3

  private def assertCloseEnough(name: String, value: Double, expected: Double): Unit = {
    assert(Math.abs(value - expected) < epsilon, s" - $name is $value, which is too far from $expected.")
  }

  private def assertRandom(random: Random, nSample: Int): Unit = {
    val stats = new Stats
    for (_ <- 1 to nSample) {
      stats.add(random.nextLong())
    }
    assertCloseEnough("mean", stats.mean, 0.0)
    assertCloseEnough("variance", stats.variance, 1.0/3.0)
    assertCloseEnough("two-point autocorrelation", stats.autoCorr2, 0.0)
    assertCloseEnough("three-point autocorrelation", stats.autoCorr3, 0.0)
  }

  test("Sample Mousse") {
    val mousse = Mousse()
    val random = mousse.asRandom
    val nSample = 5000000
    assertRandom(random, nSample)
  }
}
