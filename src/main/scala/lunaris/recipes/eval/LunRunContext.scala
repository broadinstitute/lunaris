package lunaris.recipes.eval

import lunaris.io.ResourceConfig
import lunaris.recipes.eval.LunRunContext.Observer
import org.broadinstitute.yootilz.core.snag.Snag

case class LunRunContext(resourceConfig: ResourceConfig, observer: Observer)

object LunRunContext {
  trait Observer {
    def logSnag(snag: Snag): Unit
    def logPos(seq: String, pos: Int): Unit
    def logMessage(message: String): Unit
  }

  object Observer {
    def forLogger(logger: String => Unit): PrintObserver = new PrintObserver(logger)
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
}