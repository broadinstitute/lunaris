package lunaris.io

import java.io.Closeable

import lunaris.io.Disposable.Disposer

case class Disposable[+A](a: A)(private val disposer: Disposer) {
  private var isDisposedVar: Boolean = false

  def get: A = a

  def isValid: Boolean = !isDisposedVar

  def isDisposed: Boolean = isDisposedVar

  def dispose(): Unit = {
    disposer.dispose()
    isDisposedVar = true
  }

  def useUp[B](user: A => B): B = {
    val b = user(a)
    dispose()
    b
  }

  def map[B](mapper: A => B): Disposable[B] = Disposable[B](mapper(a))(disposer)

  def flatMap[B](mapper: A => Disposable[B]): Disposable[B] = {
    val innerDis = mapper(a)
    Disposable(innerDis.a)(disposer ++ innerDis.disposer)
  }
}

object Disposable {

  def forCloseable[C <: Closeable](closeable: C): Disposable[C] =
    Disposable[C](closeable)(Disposer.ForCloseable(closeable))

  def sequence[A](disposables: Seq[Disposable[A]]): Disposable[Seq[A]] = {
    val as = disposables.map(_.a)
    val disposerCombined = Disposer.Composite(disposables.map(_.disposer))
    Disposable(as)(disposerCombined)
  }

  trait Disposer {
    def dispose(): Unit

    def ++(o: Disposer): Disposer = {
      o match {
        case Disposer.Noop =>  this
        case Disposer.Composite(disposers) => Disposer.Composite(this +: disposers)
        case _ => Disposer.Composite(Seq(this, o))
      }
    }
  }

  object Disposer {

    object Noop extends Disposer {
      override def dispose(): Unit = ()

      override def ++(o: Disposer) : Disposer = o
    }

    case class Composite(disposers: Seq[Disposer]) extends Disposer {
      override def dispose(): Unit = disposers.foreach(_.dispose())

      override def ++(o: Disposer) : Disposer = {
        o match {
          case Disposer.Noop => this
          case Disposer.Composite(oDisposers) => Disposer.Composite(disposers ++ oDisposers)
          case _ => Disposer.Composite(disposers :+ o)
        }
      }
    }

    case class ForCloseable(closeable: Closeable) extends Disposer {
      override def dispose(): Unit = closeable.close()
    }

  }

}