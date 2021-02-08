package lunaris.utils

import lunaris.utils.Mousse.{JMouseRandom, lagsDeltas}

import java.nio.charset.StandardCharsets
import java.util.{Random => JRandom}
import scala.collection.immutable.ArraySeq
import scala.util.Random

class Mousse private(n: Int, val fields: Array[Long], lags: Array[Int]) {

  var counter: Long = 0

  for (_ <- 0 to 33) { nextLong() }

  def nextLong(): Long = {
    val iFieldOld = (counter % n).toInt
    counter += 1
    val iFieldNew = (counter % n).toInt
    var next: Long = fields(iFieldOld) + 1L
    for(i <- lags.indices) {
      if(counter % lagsDeltas(i) != 0) lags(i) = (lags(i) + 1) % n
      next += fields(lags(i))
    }
    fields(iFieldNew) = next
    next
  }

  def asRandom: Random = new Random(new JMouseRandom(this))
}

object Mousse {
  val nLags: Int = 8
  val lagsDeltas: Array[Int] = Array[Int](2, 3, 5, 7, 11, 13, 17, 19)

  def apply(seed: Seq[Long] = Seq.empty, n: Int = 8): Mousse = {
    val fields = new Array[Long](n)
    val lags = new Array[Int](nLags)
    for (i <- seed.indices) {
      fields(i % n) += seed(i)
    }
    new Mousse(n, fields, lags)
  }

  def apply(string: String): Mousse =
    Mousse(ArraySeq.unsafeWrapArray(string.getBytes(StandardCharsets.UTF_8)).map(_.toLong))

  class JMouseRandom(val mousse: Mousse) extends JRandom {
    override protected def next(nBits: Int): Int = {
      (mousse.nextLong() & ((1L << nBits) -1)).toInt
    }
    override def nextLong(): Long = mousse.nextLong()
  }
}
