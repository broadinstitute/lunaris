package lunaris.utils

import org.scalatest.funsuite.AnyFunSuite

class MousseTest extends AnyFunSuite {
  test("Get some longs") {
    val mousse = Mousse()
    val random = mousse.asRandom
    for(_ <- 1 to 1000000) {
      random.nextLong()
    }
    for(i <- 1 to 20) {
      println(Seq.fill(10)(random.nextLong().toHexString).mkString(" "))
    }
  }
}
