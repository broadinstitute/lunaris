package lunaris.utils

import org.broadinstitute.yootilz.core.snag.Snag

case class SeqBasedOrdering[A](as: Seq[A]) extends Ordering[A] {
  val aToIndex: Map[A, Int] = as.zipWithIndex.toMap
  val defaultIndex: Int = aToIndex.size
  def compareOpt(a1: A, a2: A): Option[Int] = {
    (aToIndex.get(a1), aToIndex.get(a2)) match {
      case (Some(i1), Some(i2)) => Some(i1 - i2)
      case _ => None
    }
  }
  override def compare(a1: A, a2: A): Int =
    aToIndex.getOrElse(a1, defaultIndex) - aToIndex.getOrElse(a2, defaultIndex)
}

object SeqBasedOrdering {
  private def checkSeqWithSeqBasedOrdering[A](as: Seq[A], ordering: SeqBasedOrdering[A]): Option[Snag] = {
    var snagOpt: Option[Snag] = None
    val slidingIter = as.sliding(2)
    while(snagOpt.isEmpty && slidingIter.hasNext) {
      val Seq(a1, a2) = slidingIter.next()
      if(ordering.compareOpt(a1, a2).getOrElse(0) > 0) {
        snagOpt = Some(Snag(s"Inconsistent order for $a1 and $a2."))
      }
    }
    snagOpt
  }

  def combinedSeq[A](aSeqs: Iterable[Seq[A]]): Either[Snag, Seq[A]] = {
    if(aSeqs.isEmpty) {
      Left(Snag("No Seqs provided."))
    } else {
      val orderings: Seq[SeqBasedOrdering[A]] = aSeqs.toSeq.map(SeqBasedOrdering(_))
      val sumOrdering: Ordering[A] = (a1: A, a2: A) => orderings.map(_.compareOpt(a1, a2).getOrElse(0)).sum
      val aSeq: Seq[A] = aSeqs.map(_.toSet).fold(Set.empty[A])( _ ++ _).toSeq.sorted(sumOrdering)
      var snagOpt: Option[Snag] = None
      val orderingsIter = orderings.iterator
      while (snagOpt.isEmpty && orderingsIter.hasNext) {
        snagOpt = checkSeqWithSeqBasedOrdering(aSeq, orderingsIter.next())
      }
      snagOpt match {
        case Some(snag) => Left(snag)
        case None => Right(aSeq)
      }
    }
  }
}
