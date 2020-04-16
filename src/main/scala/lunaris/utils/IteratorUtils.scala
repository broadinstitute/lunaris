package lunaris.utils

import scala.collection.BufferedIterator

object IteratorUtils {

  def newBufferedIterator[A](seq: Seq[A]): SeqBufferedIterator[A] = new SeqBufferedIterator[A](seq)

  class SeqBufferedIterator[+A](seq: Seq[A]) extends BufferedIterator[A] {
    private var index: Int = 0

    override def head: A = seq(index)

    override def hasNext: Boolean = index < seq.size

    override def next(): A = {
      val a = seq(index)
      index += 1
      a
    }
  }

}
