package lunaris.utils

import org.scalatest.funsuite.AnyFunSuite

class EitheratorTest extends AnyFunSuite {

  def assertEitherator[A](etor: Eitherator[A], as: Seq[A]): Unit = {
    for (a <- as) {
      assert(etor.next() === Right(Some(a)))
    }
    assert(etor.next() === Right(None))
    assert(etor.next() === Right(None))
    assert(etor.next() === Right(None))
  }

  test("fromSeq") {
    val etor = Eitherator.fromSeq(Seq(1, 2, 3, 4, 5))
    assertEitherator(etor, Seq(1, 2, 3, 4, 5))
  }
  test("map") {
    val etor = Eitherator.fromSeq(Seq(1, 2, 3, 4, 5)).map(_ * 2)
    assertEitherator(etor, Seq(2, 4, 6, 8, 10))
  }
  test("flatMap") {
    val etor = Eitherator.fromSeq(Seq(0, 1, 2, 1, 0, 1, 2, 3)).flatMap(i => Eitherator.fromSeq(0 until i))
    assertEitherator(etor, Seq(0, 0, 1, 0, 0, 0, 1, 0, 1, 2))
  }
}
