package lunaris.utils

final class MergeCascade[T](private val merge: (T, T) => T) {

  private var items: Seq[Option[T]] = Seq.empty

  private def addToSubItems(subItems: Seq[Option[T]], item: T): Seq[Option[T]] = {
    subItems match {
      case Some(headItem) +: tail => None +: addToSubItems(tail, merge(headItem, item))
      case None +: tail => Some(item) +: tail
      case _ => Seq(Some(item))
    }
  }

  def +=(item: T): Unit = {
    items = addToSubItems(items, item)
  }

  private def mergeSubItems(subItems: Seq[Option[T]]): Option[T] = {
    subItems match {
      case Some(headItem) +: tail =>
        mergeSubItems(tail) match {
          case Some(tailMergedItem) => Some(merge(tailMergedItem, headItem))
          case None => Some(headItem)
        }
      case None +: tail => mergeSubItems(tail)
      case _ => None
    }
  }

  def resultOpt(): Option[T] = mergeSubItems(items)
}

object MergeCascade {
  def apply[T](merge: (T, T) => T): MergeCascade[T] = new MergeCascade[T](merge)
}
