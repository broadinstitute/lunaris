package lunaris.recipes.tools.builtin

import java.io.PrintWriter
import java.nio.channels.Channels
import java.nio.charset.StandardCharsets
import java.util.concurrent.TimeUnit

import akka.stream.scaladsl.{Keep, Sink, Source}
import lunaris.io.{Disposable, OutputId}
import lunaris.recipes.eval.LunRunContext.Observer
import lunaris.recipes.eval.LunWorker.RecordStreamWorker
import lunaris.recipes.eval.WorkerMaker.WorkerBox
import lunaris.recipes.eval.{LunCompileContext, LunRunContext, LunRunnable, LunWorker, WorkerMaker}
import lunaris.recipes.tools.{Tool, ToolArgUtils, ToolCall, ToolInstanceUtils}
import lunaris.recipes.values.{LunType, LunValue, LunValueJson, RecordStream}
import lunaris.recipes.{eval, tools}
import lunaris.utils.Eitherator
import org.broadinstitute.yootilz.core.snag.Snag

import scala.concurrent.Await
import scala.concurrent.duration.FiniteDuration

object JSONWriter extends Tool {
  override def resultType: LunType.UnitType.type = LunType.UnitType

  object Params {

    object Keys {
      val from = "from"
      val file = "file"
    }

    val from: Tool.RefParam = Tool.RefParam(Keys.from, LunType.RecordStreamType, isRequired = true)
    val file: Tool.ValueParam = Tool.ValueParam(Keys.file, LunType.FileType, isRequired = false)
  }

  override def params: Seq[Tool.Param] = Seq(Params.from, Params.file)

  override def isFinal: Boolean = true

  override def newToolInstance(args: Map[String, ToolCall.Arg]): Either[Snag, tools.ToolInstance] = {
    for {
      from <- ToolArgUtils.asRef(Params.Keys.from, args)
      fileOpt <- ToolArgUtils.asOutputIdOpt(Params.Keys.file, args)
    } yield ToolInstance(from, fileOpt)
  }

  case class ToolInstance(from: String,
                          fileOpt: Option[OutputId]) extends tools.ToolInstance {
    override def refs: Map[String, String] = Map(Params.Keys.from -> from)

    override def newWorkerMaker(context: LunCompileContext,
                                workers: Map[String, LunWorker]): Either[Snag, eval.WorkerMaker] = {
      ToolInstanceUtils.newWorkerMakerSingleRef(Params.Keys.from, workers){ fromWorker =>
        new WorkerMaker(fromWorker, fileOpt)
      }
    }
  }

  class WorkerMaker(fromWorker: RecordStreamWorker,
                    fileOpt: Option[OutputId]) extends eval.WorkerMaker {
    override def nOrders: Int = 0

    override def orderAnotherWorker: Either[Snag, eval.WorkerMaker.Receipt] =
      Left(Snag(s"Tool $name is final and cannot be used as input for other tools."))

    override def finalizeAndShip(): WorkerBox = new WorkerBox {
      override def pickupWorkerOpt(receipt: WorkerMaker.Receipt): Option[LunWorker] = None

      def toLineSource(recordSource: Source[LunValue.RecordValue, RecordStream.Meta], meta: RecordStream.Meta):
      Source[String, RecordStream.Meta] = {
        Source.single("{\n").concat(recordSource.map(Some(_)).concat(Source(Seq(None))).sliding(2). map { recordOpts =>
          val isLast = recordOpts.size < 2 || recordOpts(1).isEmpty
          val record = recordOpts.head.get
          val objectJsonString = LunValueJson.toJson(record)
          val idWithJson =
            "  \"" + record.id + "\" : " + objectJsonString.replace("\n", "\n  ")
          val maybeComma = if(isLast) "" else ","
          idWithJson + maybeComma
        }).concat(Source.single("}\n")).mapMaterializedValue(_ => meta)
      }

      private def writeRecords(source: Source[LunValue.RecordValue, RecordStream.Meta],
                               runContext: LunRunContext)(linePrinter: String => Unit): Unit =  {
        val doneFuture = Source.single("{").concat(source.map(Some(_)).concat(Source(Seq(None))).sliding(2). map { recordOpts =>
          val isLast = recordOpts.size < 2 || recordOpts(1).isEmpty
          val record = recordOpts.head.get
          val objectJsonString = LunValueJson.toJson(record)
          val idWithJson =
            "  \"" + record.id + "\" : " + objectJsonString.replace("\n", "\n  ")
          val maybeComma = if(isLast) "" else ","
          idWithJson + maybeComma
        }).concat(Source.single("}")).runWith(Sink.foreach(linePrinter))(runContext.materializer)
        Await.result(doneFuture, FiniteDuration(100, TimeUnit.SECONDS))
      }

      override def pickupRunnableOpt(): Option[LunRunnable] = Some[LunRunnable](new LunRunnable {
        override def execute(context: LunRunContext): Unit = {
          fromWorker.getSnagOrStreamDisposable(context).useUp {
            case Left(snag) => context.observer.logSnag(snag)
            case Right(recordStream) =>
              fileOpt match {
                case Some(file) => file.newWriteChannelDisposable(context.resourceConfig).useUp { channel =>
                  Disposable.forCloseable(new  PrintWriter(Channels.newWriter(channel, StandardCharsets.UTF_8))).useUp {
                    writer =>
                      writeRecords(recordStream.source, context)(writer.println)
                  }
                }
                case None =>
                  writeRecords(recordStream.source, context)(println)
              }
          }
        }

        override def getStream(context: LunRunContext): Disposable[Either[Snag, Source[String, RecordStream.Meta]]] =
          fromWorker.getSnagOrStreamDisposable(context).map(_.map{ recordStream =>
            toLineSource(recordStream.source, recordStream.meta)
          })
      })
    }
  }
}
