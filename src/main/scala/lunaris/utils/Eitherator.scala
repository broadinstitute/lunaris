package lunaris.utils

import lunaris.utils.Eitherator.MappedEitherator
import org.broadinstitute.yootilz.core.snag.Snag

trait Eitherator[+A] {
  def next(): Either[Snag, Option[A]]

  def map[B](fun: A => B): MappedEitherator[A, B] = new MappedEitherator[A, B](this)(fun)

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

  class MappedEitherator[+A, B](underlying: Eitherator[A])(fun: A => B) extends Eitherator[B] {
    override def next(): Either[Snag, Option[B]] = underlying.next().map(_.map(fun))
  }

}
