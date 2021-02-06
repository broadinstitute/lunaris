package lunaris.utils

import org.scalatest.funsuite.AnyFunSuite

class MousseTest extends AnyFunSuite {
  test("Get some longs") {
    val mousse = Mousse(8)
    for(_ <- 1 to 1000000) {
      mousse.nextLong()
    }
    for(i <- 1 to 20) {
      println(Seq.fill(10)(mousse.nextLong().toHexString).mkString(" "))
    }
  }
}
