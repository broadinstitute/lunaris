package lunaris.utils

import lunaris.utils.Mousse.lagsDeltas

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
}

object Mousse {
  val nLags: Int = 8
  val lagsDeltas: Array[Int] = Array[Int](2, 3, 5, 7, 11, 13, 17, 19)

  def apply(n: Int, seed: Seq[Long] = Seq.empty): Mousse = {
    val fields = new Array[Long](n)
    val lags = new Array[Int](nLags)
    for (i <- seed.indices) {
      fields(i % n) += seed(i)
    }
    new Mousse(n, fields, lags)
  }
}
