package lunaris.utils

import org.broadinstitute.yootilz.core.snag.Snag

object EitherSeqUtils {
  def foreach[A, B](xs: Seq[A])(f: A => Either[Snag, B]): Either[Snag, Unit] = {
    val i = xs.iterator
    while (i.hasNext) f(i.next) match {
      case Right(_) => ()
      case Left(e) => return Left(e)
    }
    Right(())
  }

  def repeat[A](n: Int)(fun: => Either[Snag, A]): Either[Snag, Unit] = {
    var snagOpt: Option[Snag] = None
    var i: Int = 0
    while (snagOpt.isEmpty && i < n) {
      fun match {
        case Left(snag) => snagOpt = Some(snag)
        case Right(_) => ()
      }
      i += 1
    }
    snagOpt match {
      case Some(snag) => Left(snag)
      case None => Right(())
    }
  }

  def traverse[A, B](xs: Seq[A])(f: A => Either[Snag, B]): Either[Snag, Seq[B]] = {
    val builder = Seq.newBuilder[B]
    val iter = xs.iterator
    while (iter.hasNext) {
      f(iter.next) match {
        case Right(b) => builder += b
        case Left(snag) => return Left(snag)
      }
    }
    Right(builder.result)
  }

  def fill[A](n: Int)(f: => Either[Snag, A]): Either[Snag, Seq[A]] = {
    val builder = Seq.newBuilder[A]
    var i: Int = 0
    while (i < n) {
      f match {
        case Right(b) => builder += b
        case Left(snag) => return Left[Snag, Seq[A]](snag)
      }
      i += 1
    }
    Right(builder.result)
  }
}
