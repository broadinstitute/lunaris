package lunaris.recipes.eval

import lunaris.recipes.eval.LunRunnable.Observer
import org.broadinstitute.yootilz.core.snag.Snag

trait LunRunnable {
  def execute(observer: Observer): Unit
}

object LunRunnable {

  def combine(runnables: Iterable[LunRunnable]): LunRunnable = {
    runnables.size match {
      case 0 => NoOpRunnable
      case 1 => runnables.head
      case _ => CompositeRunnable(runnables)
    }
  }

  trait Observer {
    def logSnag(snag: Snag): Unit
    def logPos(seq: String, pos: Int): Unit
    def logMessage(message: String): Unit
  }

  object NoOpObserver extends Observer {
    override def logSnag(snag: Snag): Unit = ()

    override def logPos(seq: String, pos: Int): Unit = ()

    override def logMessage(message: String): Unit = ()
  }

  class PrintObserver(logger: String => Unit) extends Observer {
    override def logSnag(snag: Snag): Unit = {
      logger("Problem!")
      logger(snag.message)
      if(snag.message != snag.report) {
        logger(snag.report)
      }
    }

    override def logPos(seq: String, pos: Int): Unit = logger(s"Now at $seq:$pos.")

    override def logMessage(message: String): Unit = logger(message)
  }

  object NoOpRunnable extends LunRunnable {
    override def execute(observer: Observer): Unit = ()
  }

  case class CompositeRunnable(runnables: Iterable[LunRunnable]) extends LunRunnable {
    override def execute(observer: Observer): Unit = runnables.foreach(_.execute(observer))
  }
}

