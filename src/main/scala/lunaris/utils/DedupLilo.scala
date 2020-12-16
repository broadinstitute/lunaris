package lunaris.utils

final class DedupLilo[T](val nItemsMax: Int) {
  var items: Vector[T] = Vector.empty[T]
  var itemSet: Set[T] = Set.empty

  def add(item: T): DedupLilo.Result[T] = {
    if (itemSet(item)) {
      val iItem = items.indexOf(item)
      items = Vector(item) ++ items.slice(0, iItem) ++ items.slice(iItem + 1, items.size)
      DedupLilo.Result.ExistingItem
    } else {
      if (items.size < nItemsMax) {
        items = item +: items
        itemSet = itemSet + item
        DedupLilo.Result.NewItem
      } else {
        val overflowItem = items.last
        items = item +: items.slice(0, nItemsMax - 1)
        itemSet = itemSet - overflowItem + item
        DedupLilo.Result.Overflow(overflowItem)
      }
    }
  }

  def clear(): Unit = {
    items = Vector.empty
    itemSet = Set.empty
  }
}

object DedupLilo {
  def apply[T](nItemsMax: Int): DedupLilo[T] = new DedupLilo[T](nItemsMax)

  sealed trait Result[+T]

  object Result {

    sealed trait NonOverflow extends Result[Nothing]

    object NewItem extends NonOverflow

    object ExistingItem extends NonOverflow

    final case class Overflow[T](oldestItem: T) extends Result[T]

  }

}
