package lunaris.recipes.tools.native

import java.io.PrintWriter
import java.nio.channels.Channels
import java.nio.charset.StandardCharsets
import java.util.concurrent.TimeUnit

import akka.NotUsed
import akka.stream.scaladsl.{Keep, Sink, Source}
import lunaris.io.{Disposable, OutputId}
import lunaris.recipes.eval.LunWorker.RecordStreamWorker
import lunaris.recipes.eval.WorkerMaker.WorkerBox
import lunaris.recipes.eval.{LunCompileContext, LunRunContext, LunRunnable, LunWorker, WorkerMaker}
import lunaris.recipes.tools.{Tool, ToolArgUtils, ToolCall, ToolInstanceUtils}
import lunaris.recipes.values.LunType.RecordType
import lunaris.recipes.values.LunValue.RecordValue
import lunaris.recipes.values.{LunType, LunValue, LunValueJson, RecordStream}
import lunaris.recipes.{eval, tools}
import org.broadinstitute.yootilz.core.snag.Snag

import scala.concurrent.Await
import scala.concurrent.duration.FiniteDuration

object TSVWriter extends Tool {
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
      ToolInstanceUtils.newWorkerMakerSingleRef(Params.Keys.from, workers) { fromWorker =>
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

      private def headerLine(objectType: RecordType): String = "#" + objectType.fields.mkString("\t")

      private def valueToString(value: LunValue): String = {
        value match {
          case primitiveValue: LunValue.PrimitiveValue => primitiveValue.value.toString
          case _ => LunValueJson.valueEncoder.apply(value).noSpaces
        }
      }

      private def dataLine(objectValue: RecordValue): String = {
        objectValue.lunType.fields.map(objectValue.values.get).map(_.map(valueToString))
          .map(_.getOrElse("")).mkString("\t")
      }

      private def writeStreamAsTsv(stream: RecordStream, context: LunRunContext)(writer: String => Unit): Unit = {
        writer(headerLine(stream.meta.objectType))
        val doneFuture = stream.source.map(dataLine).runWith(Sink.foreach(writer))(context.materializer)
        Await.result(doneFuture, FiniteDuration(100, TimeUnit.SECONDS))
      }

      override def pickupRunnableOpt(): Option[LunRunnable] = Some[LunRunnable](new LunRunnable {
        override def execute(context: LunRunContext): Unit = {
          fromWorker.getSnagOrStreamDisposable(context).useUp {
            case Left(snag) => context.observer.logSnag(snag)
            case Right(headerAndRecordEtor) =>
              fileOpt match {
                case Some(file) => file.newWriteChannelDisposable(context.resourceConfig).useUp { channel =>
                  Disposable.forCloseable(new PrintWriter(Channels.newWriter(channel, StandardCharsets.UTF_8))).useUp {
                    writer => writeStreamAsTsv(headerAndRecordEtor, context)(writer.println)
                  }
                }
                case None => writeStreamAsTsv(headerAndRecordEtor, context)(println)
              }
          }
        }

        override def getStream(context: LunRunContext): Disposable[Either[Snag, Source[String, RecordStream.Meta]]] =
          fromWorker.getSnagOrStreamDisposable(context).map(_.map { recordStream =>
            val meta = recordStream.meta
            Source.single(headerLine(meta.objectType)).concat(recordStream.source.map(dataLine))
              .mapMaterializedValue(_ => meta)
          })
      })
    }
  }

}
