package lunaris.utils

import akka.NotUsed
import akka.stream.Materializer
import akka.stream.scaladsl.{Keep, Sink, SinkQueue, Source}
import lunaris.recipes.values.RecordStream
import lunaris.utils.Eitherator.EitheratorIterator
import org.broadinstitute.yootilz.core.snag.Snag

import scala.concurrent.Await
import scala.concurrent.duration.{Duration, SECONDS}
import scala.util.control.NonFatal

object EitheratorStreamsInterop {

  def eitheratorToStream[A](etor: Eitherator[A], meta: RecordStream.Meta): Source[A, RecordStream.Meta] = {
    Source.fromIterator(() => new EitheratorIterator[A](etor)).mapMaterializedValue(_ => meta)
  }

  def streamToEitherator[A, M](source: Source[A, M])(implicit materializer: Materializer): Eitherator[A] = {
    val sink = Sink.queue[A]()
    val matSink = source.toMat(sink)(Keep.right).run
    new SinkQueueEitherator[A](matSink)
  }

  class SinkQueueEitherator[A](sinkQueue: SinkQueue[A]) extends Eitherator[A] {
    var snagOpt: Option[Snag] = None

    override def next(): Either[Snag, Option[A]] = {
      snagOpt match {
        case Some(snag) => Left(snag)
        case None =>
          try {
            Right(Await.result(sinkQueue.pull(), Duration(100, SECONDS)))
          } catch {
            case NonFatal(ex) =>
              val snag = Snag(ex)
              snagOpt = Some(snag)
              Left(snag)
          }
      }
    }
  }

}
