package lunaris.utils

import org.broadinstitute.yootilz.core.snag.Snag

object EitherSeqUtils {
  def sequence[A](snagOrAs: Seq[Either[Snag, A]]): Either[Snag, Seq[A]] = {
    snagOrAs.find(_.isLeft) match  {
      case Some(left) => Left(left.left.toOption.get)
      case None => Right(snagOrAs.map(_.toOption.get))
    }
  }

  def foreach[A, B](xs: Seq[A])(f: A => Either[Snag, B]): Either[Snag, Unit] = {
    val i = xs.iterator
    while (i.hasNext) f(i.next()) match {
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
      f(iter.next()) match {
        case Right(b) => builder += b
        case Left(snag) => return Left(snag)
      }
    }
    Right(builder.result())
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
    Right(builder.result())
  }

  def traverseMap[K, V, W](map: Map[K, V])(f: V => Either[Snag, W]): Either[Snag, Map[K, W]] = {
   val builder = Map.newBuilder[K, W]
    val iter = map.iterator
    while(iter.hasNext) {
      val (key, value) = iter.next()
      f(value) match {
        case Left(snag) => return Left(snag)
        case Right(mappedValue) => builder += (key -> mappedValue)
      }
    }
    Right(builder.result())
  }
}
