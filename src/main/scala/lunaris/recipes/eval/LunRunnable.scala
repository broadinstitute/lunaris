package lunaris.recipes.eval

import akka.stream.scaladsl.Source
import lunaris.recipes.eval.LunRunnable.RunResult
import lunaris.recipes.values.RecordStreamWithMeta
import org.broadinstitute.yootilz.core.snag.Snag

import scala.collection.immutable.Iterable
import scala.concurrent.Future

trait LunRunnable {
  def executeAsync(context: LunRunContext, snagTracker: SnagTracker): Future[RunResult]

  def getStream(context: LunRunContext, snagTracker: SnagTracker):
  Either[Snag, Source[String, RecordStreamWithMeta.Meta]]
}

object LunRunnable {

  case class RunResult(snags: Seq[Snag]) {
    def ++(oResult: RunResult): RunResult = RunResult(snags ++ oResult.snags)
  }

  object RunResult {
    def apply(snag: Snag): RunResult = RunResult(Seq(snag))
    def create: RunResult = RunResult(Seq.empty)
  }

  def combine(runnables: Iterable[LunRunnable]): LunRunnable = {
    runnables.size match {
      case 0 => NoOpRunnable
      case 1 => runnables.head
      case _ => CompositeRunnable(runnables)
    }
  }

  object NoOpRunnable extends LunRunnable {
    override def executeAsync(context: LunRunContext, snagTracker: SnagTracker): Future[RunResult] =
      Future(RunResult.create)(context.materializer.executionContext)

    override def getStream(context: LunRunContext, snagTracker: SnagTracker):
    Either[Snag, Source[String, RecordStreamWithMeta.Meta]] =
      Left(Snag("Cannot get a stream from a NoOpRunnable."))
  }

  case class CompositeRunnable(runnables: Iterable[LunRunnable]) extends LunRunnable {
    override def executeAsync(context: LunRunContext, snagTracker: SnagTracker): Future[RunResult] = {
      val unitFuts = runnables.map(_.executeAsync(context, snagTracker))
      Future.foldLeft(unitFuts)(RunResult(snagTracker.buildSeq()))(_ ++ _)(context.materializer.executionContext)
    }

    override def getStream(context: LunRunContext, snagTracker: SnagTracker):
    Either[Snag, Source[String, RecordStreamWithMeta.Meta]] = {
      if (runnables.isEmpty) {
        Left(Snag("No streams available"))
      } else if (runnables.size == 1) {
        runnables.head.getStream(context, snagTracker)
      } else {
        Left(Snag("Don't know how to combine multiple output streams."))
      }
    }
  }

}

