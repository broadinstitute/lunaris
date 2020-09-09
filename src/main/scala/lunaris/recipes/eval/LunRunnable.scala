package lunaris.recipes.eval

import akka.Done
import akka.stream.scaladsl.Source
import lunaris.io.Disposable
import lunaris.recipes.values.RecordStreamWithMeta
import org.broadinstitute.yootilz.core.snag.Snag

import scala.collection.immutable.Iterable
import scala.concurrent.Future

trait LunRunnable {
  def execute(context: LunRunContext): Unit

  def executeAsync(context: LunRunContext): Future[Done]

  def getStream(context: LunRunContext): Disposable[Either[Snag, Source[String, RecordStreamWithMeta.Meta]]]
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

    override def executeAsync(context: LunRunContext): Future[Done] =
      Future(Done)(context.materializer.executionContext)

    override def getStream(context: LunRunContext): Disposable[Either[Snag, Source[String, RecordStreamWithMeta.Meta]]] =
      Disposable(Left(Snag("Nothing to do")))(Disposable.Disposer.Noop)
  }

  case class CompositeRunnable(runnables: Iterable[LunRunnable]) extends LunRunnable {
    override def execute(context: LunRunContext): Unit = runnables.foreach(_.execute(context))

    override def executeAsync(context: LunRunContext): Future[Done] = {
      val unitFuts = runnables.map(_.executeAsync(context))
      Future.foldLeft[Done, Done](unitFuts)(Done)((_, _) => Done)(context.materializer.executionContext)
    }

    override def getStream(context: LunRunContext):
    Disposable[Either[Snag, Source[String, RecordStreamWithMeta.Meta]]] = {
      if (runnables.isEmpty) {
        Disposable(Left(Snag("Nothing to do")))(Disposable.Disposer.Noop)
      } else if (runnables.size == 1) {
        runnables.head.getStream(context)
      } else {
        Disposable(Left(Snag("Don't know how to combine multiple output streams.")))(Disposable.Disposer.Noop)
      }
    }
  }

}

