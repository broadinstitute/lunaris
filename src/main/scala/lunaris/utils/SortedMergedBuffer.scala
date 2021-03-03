package lunaris.utils

final class SortedMergedBuffer[T](private val ordering: Ordering[T])(private val canMerge: (T, T) => Boolean)
                                 (private val merge: (T, T) => T) {

  private val sortedSeqsMerger: (Seq[T], Seq[T]) => Seq[T] = { (seq1, seq2) =>
    val iter1 = seq1.iterator
    val iter2 = seq2.iterator
    var itemOpt1: Option[T] = iter1.nextOption()
    var itemOpt2: Option[T] = iter2.nextOption()
    var itemOptOut: Option[T] = None
    val mergedSeqBuilder = Seq.newBuilder[T]
    var keepGoing: Boolean = true
    while(keepGoing) {
      (itemOpt1, itemOpt2, itemOptOut) match {
        case (Some(item1), _, Some(itemOut)) if canMerge(item1, itemOut) =>
          itemOptOut = Some(merge(item1, itemOut))
          itemOpt1 = iter1.nextOption()
        case (_, Some(item2), Some(itemOut)) if canMerge(item2, itemOut) =>
          itemOptOut = Some(merge(item2, itemOut))
          itemOpt2 = iter2.nextOption()
        case (_, _, Some(itemOut)) =>
          mergedSeqBuilder += itemOut
          itemOptOut = None
        case (Some(item1), Some(item2), None) =>
          if(canMerge(item1, item2)) {
            itemOptOut = Some(merge(item1, item2))
            itemOpt1 = iter1.nextOption()
            itemOpt2 = iter2.nextOption()
          } else {
            if(ordering.compare(item1, item2) <= 0) {
              itemOptOut = Some(item1)
              itemOpt1 = iter1.nextOption()
            } else {
              itemOptOut = Some(item2)
              itemOpt2 = iter2.nextOption()
            }
          }
        case (Some(item1), None, None) =>
          itemOptOut = Some(item1)
          itemOpt1 = iter1.nextOption()
        case (None, Some(item2), None) =>
          itemOptOut = Some(item2)
          itemOpt2 = iter2.nextOption()
        case (None, None, None) =>
          keepGoing = false
      }
    }
    mergedSeqBuilder.result()
  }

  private val mergeCascade = MergeCascade[Seq[T]](sortedSeqsMerger)

  def +=(item: T): Unit = {
    mergeCascade += Seq(item)
  }

  def resultOpt(): Option[Seq[T]] = mergeCascade.resultOpt()
}

object SortedMergedBuffer {
  def apply[T](ordering: Ordering[T])(canMerge: (T, T) => Boolean)(merge: (T, T) => T): SortedMergedBuffer[T] =
    new SortedMergedBuffer[T](ordering)(canMerge)(merge)
}