package lunaris.utils

import lunaris.utils.Eitherator.{CollectEitherator, FilterEitherator, FlatMappedEitherator, MappedEitherator, ProcessorEitherator}
import org.broadinstitute.yootilz.core.snag.Snag

trait Eitherator[+A] {
  def next(): Either[Snag, Option[A]]

  def map[B](fun: A => B): MappedEitherator[A, B] = new MappedEitherator[A, B](this)(fun)

  def filter(pred: A => Boolean): FilterEitherator[A] = new FilterEitherator[A](this)(pred)

  def flatMap[B](fun: A => Eitherator[B]): Eitherator[B] = new FlatMappedEitherator(this)(fun)

  def foreach(user: A => Unit): Either[Snag, Option[A]] = {
    var lastNext: Either[Snag, Option[A]] = Right(None)
    do {
      lastNext = next()
      lastNext.foreach(_.foreach(user))
    } while (lastNext.fold(_ => false, _.nonEmpty))
    lastNext
  }

  def snagOrForeach[B](handler: Snag => B)(user: A => B): Option[B] = {
    var bOpt: Option[B] = None
    var keepGoing: Boolean = true
    do {
      next() match {
        case Left(snag) =>
          handler(snag)
          keepGoing = false
        case Right(Some(a)) =>
          bOpt = Some(user(a))
        case Right(None) =>
          keepGoing = false
      }
    } while (keepGoing)
    bOpt
  }

  def collect[B](fun: A => Option[B]): CollectEitherator[A, B] = new CollectEitherator[A, B](this)(fun)

  def process[B](processor: A => Either[Snag, Option[B]]): ProcessorEitherator[A, B] =
    new ProcessorEitherator[A, B](this)(processor)
}

object Eitherator {

  def empty[A]: Eitherator[A] = Empty

  object Empty extends Eitherator[Nothing] {
    override def next(): Either[Snag, Option[Nothing]] = Right(None)
  }

  def fromGenerator[A](cond: => Boolean)(generator: => Either[Snag, A]): Eitherator[A] =
    new GeneratorEitherator[A](cond)(generator)

  class GeneratorEitherator[+A](cond: => Boolean)(generator: => Either[Snag, A]) extends Eitherator[A] {
    var snagOpt: Option[Snag] = None

    override def next(): Either[Snag, Option[A]] = {
      snagOpt match {
        case Some(snag) =>
          snagOpt = Some(snag)
          Left(snag)
        case None =>
          if (cond) {
            generator.map(Some(_))
          } else {
            Right(None)
          }
      }
    }
  }

  def forSnag[A](snag: Snag): Eitherator[A] = new SnaggedEitherator[A](snag)

  case class SnaggedEitherator[+A](snag: Snag) extends Eitherator[A] {
    override def next(): Either[Snag, Option[A]] = Left(snag)
  }

  def singleValue[A](a: A): SingleValueEitherator[A] = new SingleValueEitherator[A](a)

  class SingleValueEitherator[+A](val a: A) extends Eitherator[A] {
    var hasNext: Boolean = true

    override def next(): Either[Snag, Option[A]] = {
      if (hasNext) {
        hasNext = false
        Right(Some(a))
      } else {
        Right(None)
      }
    }
  }

  object SingleValueEitherator {
    def apply[A](a: A): SingleValueEitherator[A] = new SingleValueEitherator[A](a)
  }

  def fromSeq[A](seq: Seq[A]): IteratorEitherator[A] = new IteratorEitherator[A](seq.iterator)

  class IteratorEitherator[+A](iterator: Iterator[A]) extends Eitherator[A] {
    override def next(): Either[Snag, Option[A]] = {
      if (iterator.hasNext) {
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
      while (snagOpt.isEmpty && !exhausted && valueOpt.isEmpty) {
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

  class FilterEitherator[+A](underlying: Eitherator[A])(pred: A => Boolean) extends Eitherator[A] {
    var snagOpt: Option[Snag] = None
    var underlyingExhausted: Boolean = false
    override def next(): Either[Snag, Option[A]] = {
      var aOpt: Option[A] = None
      while(snagOpt.isEmpty && aOpt.isEmpty && !underlyingExhausted) {
        underlying.next() match {
          case Left(snag) => snagOpt = Some(snag)
          case Right(Some(a)) => if(pred(a)) aOpt = Some(a)
          case Right(None) => underlyingExhausted = true
        }
      }
      snagOpt match {
        case Some(snag) => Left(snag)
        case None => Right(aOpt)
      }
    }
  }

  class CollectEitherator[+A, B](underlying: Eitherator[A])(fun: A => Option[B]) extends Eitherator[B] {
    override def next(): Either[Snag, Option[B]] = {
      var snagOpt: Option[Snag] = None
      var bOpt: Option[B] = None
      var underlyingExhausted: Boolean = false
      while(snagOpt.isEmpty && bOpt.isEmpty && !underlyingExhausted) {
        underlying.next() match {
          case Left(snag) => snagOpt = Some(snag)
          case Right(Some(a)) => bOpt = fun(a)
          case Right(None) => underlyingExhausted = true
        }
      }
      snagOpt match {
        case Some(snag) => Left(snag)
        case None => Right(bOpt)
      }
    }
  }

  class ProcessorEitherator[+A, B](underlying: Eitherator[A])(processor: A => Either[Snag, Option[B]])
    extends Eitherator[B] {
    var snagOpt: Option[Snag] = None
    var underlyingExhausted: Boolean = false
    override def next(): Either[Snag, Option[B]] = {
      snagOpt match {
        case Some(snag) => Left(snag)
        case None =>
          var bOpt: Option[B] = None
          while(snagOpt.isEmpty && bOpt.isEmpty && !underlyingExhausted) {
            underlying.next() match {
              case Left(snag) => snagOpt = Some(snag)
              case Right(Some(a)) =>
                processor(a) match {
                  case Left(snag) => snagOpt = Some(snag)
                  case Right(Some(b)) => bOpt = Some(b)
                  case Right(None) => ()
                }
              case Right(None) => underlyingExhausted = true
            }
          }
          snagOpt match {
            case Some(snag) => Left(snag)
            case None => Right(bOpt)
          }
      }
    }
  }
}
