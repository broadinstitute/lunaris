package lunaris.recipes.eval

import akka.stream.scaladsl.Source
import lunaris.io.Disposable
import lunaris.recipes.values.RecordStream
import org.broadinstitute.yootilz.core.snag.Snag

trait LunRunnable {
  def execute(context: LunRunContext): Unit
  def getStream(context: LunRunContext): Disposable[Either[Snag, Source[String, RecordStream.Meta]]]
}

object LunRunnable {

  def combine(runnables: Iterable[LunRunnable]): LunRunnable = {
    runnables.size match {
      case 0 => NoOpRunnable
      case 1 => runnables.head
      case _ => CompositeRunnable(runnables)
    }
  }

  object NoOpRunnable extends LunRunnable {
    override def execute(context: LunRunContext): Unit = ()

    override def getStream(context: LunRunContext): Disposable[Either[Snag, Source[String, RecordStream.Meta]]] =
      Disposable(Left(Snag("Nothing to do")))(Disposable.Disposer.Noop)
  }

  case class CompositeRunnable(runnables: Iterable[LunRunnable]) extends LunRunnable {
    override def execute(context: LunRunContext): Unit = runnables.foreach(_.execute(context))

    override def getStream(context: LunRunContext): Disposable[Either[Snag, Source[String, RecordStream.Meta]]] = {
      if(runnables.isEmpty) {
        Disposable(Left(Snag("Nothing to do")))(Disposable.Disposer.Noop)
      } else if(runnables.size == 1) {
        runnables.head.getStream(context)
      } else {
        Disposable(Left(Snag("Don't know how to combine multiple output streams.")))(Disposable.Disposer.Noop)
      }
    }
  }
}

