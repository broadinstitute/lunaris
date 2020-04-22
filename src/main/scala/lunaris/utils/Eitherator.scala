package lunaris.utils

import lunaris.utils.Eitherator.{FlatMappedEitherator, MappedEitherator}
import org.broadinstitute.yootilz.core.snag.Snag

trait Eitherator[+A] {
  def next(): Either[Snag, Option[A]]

  def map[B](fun: A => B): MappedEitherator[A, B] = new MappedEitherator[A, B](this)(fun)

  def flatMap[B](fun: A => Eitherator[B]): Eitherator[B] = new FlatMappedEitherator(this)(fun)

  def foreach(user: A => Unit): Either[Snag, Option[A]] = {
    var lastNext: Either[Snag, Option[A]] = Right(None)
    do {
      lastNext = next()
      lastNext.foreach(_.foreach(user))
    } while (lastNext.fold(_ => false, _.nonEmpty))
    lastNext
  }

}

object Eitherator {

  def empty[A]: Eitherator[A] = Empty

  object Empty extends Eitherator[Nothing] {
    override def next(): Either[Snag, Option[Nothing]] = Right(None)
  }

  def singleton[A](a: A): SingleEitherator[A] = new SingleEitherator[A](a)

  class SingleEitherator[+A](val a: A) extends Eitherator[A] {
    var hasNext: Boolean = true
    override def next(): Either[Snag, Option[A]] = {
      if(hasNext) {
        hasNext = false
        Right(Some(a))
      } else {
        Right(None)
      }
    }
  }

  object SingleEitherator {
    def apply[A](a: A): SingleEitherator[A] = new SingleEitherator[A](a)
  }

  def fromSeq[A](seq: Seq[A]): IteratorEitherator[A] = new IteratorEitherator[A](seq.iterator)

  class IteratorEitherator[+A](iterator: Iterator[A]) extends Eitherator[A] {
    override def next(): Either[Snag, Option[A]] = {
      if(iterator.hasNext) {
        Right(Some(iterator.next()))
      } else {
        Right(None)
      }
    }
  }

  class MappedEitherator[+A, B](underlying: Eitherator[A])(fun: A => B) extends Eitherator[B] {
    override def next(): Either[Snag, Option[B]] = underlying.next().map(_.map(fun))
  }

  class FlatMappedEitherator[+A, B](underlying: Eitherator[A])(fun: A => Eitherator[B]) extends Eitherator[B] {
    var snagOpt: Option[Snag] = None
    var exhausted: Boolean = false
    var subEitheratorOpt: Option[Eitherator[B]] = None

    private def newSubEitherator(): Unit = {
      underlying.next() match {
        case Left(snag) =>
          snagOpt = Some(snag)
        case Right(Some(a)) =>
          subEitheratorOpt = Some(fun(a))
        case Right(None) =>
          exhausted = true
      }
    }

    override def next(): Either[Snag, Option[B]] = {
      var valueOpt: Option[B] = None
      while(snagOpt.isEmpty && !exhausted && valueOpt.isEmpty) {
        subEitheratorOpt match {
          case Some(subEitherator) =>
            subEitherator.next() match {
              case Left(snag) =>
                snagOpt = Some(snag)
              case Right(Some(b)) =>
                valueOpt = Some(b)
              case Right(None) =>
                newSubEitherator()
            }
          case None =>
            newSubEitherator()
        }
      }
      snagOpt match {
        case Some(snag) => Left(snag)
        case None => Right(valueOpt)
      }
    }
  }
}
