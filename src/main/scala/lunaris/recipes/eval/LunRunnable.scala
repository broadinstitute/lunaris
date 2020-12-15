package lunaris.recipes.eval

import akka.stream.scaladsl.{Sink, Source}
import lunaris.io.OutputId
import lunaris.recipes.eval.LunRunnable.RunResult
import lunaris.recipes.eval.LunWorker.RecordStreamWorker
import lunaris.recipes.values.RecordStreamWithMeta
import lunaris.streams.utils.RecordStreamTypes.Record
import org.broadinstitute.yootilz.core.snag.Snag

import java.io.PrintWriter
import java.nio.channels.Channels
import java.nio.charset.StandardCharsets
import scala.collection.immutable.Iterable
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor, Future}

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

  class TextWriter(fromWorker: RecordStreamWorker, outputIdOpt: Option[OutputId])(
    recordsToLines: RecordStreamWithMeta => Source[String, RecordStreamWithMeta.Meta]
  )
  extends LunRunnable {
    private def writeRecords(stream: RecordStreamWithMeta,
                             context: LunRunContext,
                             snagTracker: SnagTracker)(
                              writer: String => Unit
                            )(implicit executor: ExecutionContext): Future[RunResult] = {
      recordsToLines(stream).runWith(Sink.foreach(writer))(context.materializer)
        .map(_ => RunResult(snagTracker.buildSeq()))
    }


    override def executeAsync(context: LunRunContext, snagTracker: SnagTracker): Future[RunResult] = {
      implicit val executionContext: ExecutionContextExecutor = context.materializer.executionContext
      fromWorker.getStreamBox(context, snagTracker).snagOrStream match {
        case Left(snag) =>
          Future {
            snagTracker.trackSnag(snag)
            RunResult(snagTracker.buildSeq())
          }
        case Right(recordStreamWithMeta) =>
          outputIdOpt match {
            case Some(file) =>
              val writeChannelDisp = file.newWriteChannelDisposable(context.resourceConfig)
              val channel = writeChannelDisp.a
              val writer = new PrintWriter(Channels.newWriter(channel, StandardCharsets.UTF_8))
              val doneFut = writeRecords(recordStreamWithMeta, context, snagTracker)(writer.println)
              doneFut.onComplete(_ => writer.close())
              doneFut
            case None => writeRecords(recordStreamWithMeta, context, snagTracker)(println)
          }
      }
    }

    override def getStream(context: LunRunContext, snagTracker: SnagTracker):
    Either[Snag, Source[String, RecordStreamWithMeta.Meta]] = {
      fromWorker.getStreamBox(context,snagTracker).snagOrStream.map { recordStream =>
        recordsToLines(recordStream)
      }
    }
  }

}

