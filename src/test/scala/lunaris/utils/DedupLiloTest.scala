package lunaris.utils

import org.scalatest.funsuite.AnyFunSuite

class DedupLiloTest extends AnyFunSuite {

  def addAndAssert(dedupLilo: DedupLilo[String], newItem: String, expectedResult: DedupLilo.Result[String],
                   expectedItems: Vector[String]): Unit = {
    assert(dedupLilo.add(newItem) == expectedResult)
    assert(dedupLilo.items == expectedItems)
  }

  test("Adding items") {
    val dedupLilo = DedupLilo[String](3)
    for (_ <- 1 to 3) {
      assert(dedupLilo.items == Vector.empty)
      assert(dedupLilo.itemSet == Set.empty)
      addAndAssert(dedupLilo, "foo", DedupLilo.Result.NewItem, Vector("foo"))
      addAndAssert(dedupLilo, "foo", DedupLilo.Result.ExistingItem, Vector("foo"))
      addAndAssert(dedupLilo, "bar", DedupLilo.Result.NewItem, Vector("bar", "foo"))
      addAndAssert(dedupLilo, "foo", DedupLilo.Result.ExistingItem, Vector("foo", "bar"))
      addAndAssert(dedupLilo, "bar", DedupLilo.Result.ExistingItem, Vector("bar", "foo"))
      addAndAssert(dedupLilo, "baz", DedupLilo.Result.NewItem, Vector("baz", "bar", "foo"))
      addAndAssert(dedupLilo, "boom", DedupLilo.Result.Overflow("foo"), Vector("boom", "baz", "bar"))
      addAndAssert(dedupLilo, "bang", DedupLilo.Result.Overflow("bar"), Vector("bang", "boom", "baz"))
      addAndAssert(dedupLilo, "baz", DedupLilo.Result.ExistingItem, Vector("baz", "bang", "boom"))
      addAndAssert(dedupLilo, "bang", DedupLilo.Result.ExistingItem, Vector("bang", "baz", "boom"))
      addAndAssert(dedupLilo, "foo", DedupLilo.Result.Overflow("boom"), Vector("foo", "bang", "baz"))
      dedupLilo.clear()
    }
  }

}
