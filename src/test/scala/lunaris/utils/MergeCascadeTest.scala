package lunaris.utils

import org.scalatest.funsuite.AnyFunSuite

final class MergeCascadeTest extends AnyFunSuite {
  test("Basic") {
    var expected: String = ""
    val mergeCascade = new MergeCascade[String](_ + _)
    def add(string: String): Unit = {
      mergeCascade += string
      expected += string
      assert(mergeCascade.resultOpt().contains(expected))
    }
    "Hello, World".split("").foreach(add)
  }

}
