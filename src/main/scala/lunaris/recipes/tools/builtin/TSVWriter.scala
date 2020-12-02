package lunaris.recipes.tools.builtin

import akka.stream.scaladsl.{Keep, Sink, Source}
import lunaris.io.OutputId
import lunaris.recipes.eval.LunRunnable.RunResult
import lunaris.recipes.eval.LunWorker.RecordStreamWorker
import lunaris.recipes.eval.WorkerMaker.WorkerBox
import lunaris.recipes.eval._
import lunaris.recipes.tools.{Tool, ToolArgUtils, ToolCall, ToolInstanceUtils}
import lunaris.recipes.values.LunType.RecordType
import lunaris.recipes.values.LunValue.RecordValue
import lunaris.recipes.values.{LunType, LunValue, LunValueJson, RecordStreamWithMeta}
import lunaris.recipes.{eval, tools}
import org.broadinstitute.yootilz.core.snag.Snag

import java.io.PrintWriter
import java.nio.channels.Channels
import java.nio.charset.StandardCharsets
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor, Future}

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
      ToolInstanceUtils.newWorkerMaker1(Params.Keys.from, workers) { fromWorker =>
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

      private def getLineStream(recordStream: RecordStreamWithMeta): Source[String, RecordStreamWithMeta.Meta] = {
        val meta = recordStream.meta
        Source.single(headerLine(meta.objectType)).concatMat(recordStream.source.map(dataLine))(Keep.right)
      }

      private def writeStreamAsTsv(stream: RecordStreamWithMeta,
                                   context: LunRunContext,
                                   snagTracker: SnagTracker)(
        writer: String => Unit
      )(implicit executor: ExecutionContext): Future[RunResult] = {
        getLineStream(stream).runWith(Sink.foreach(writer))(context.materializer)
          .map(_ => RunResult(snagTracker.buildSeq()))
      }

      override def pickupRunnableOpt(): Option[LunRunnable] = Some[LunRunnable](new LunRunnable {
        override def executeAsync(context: LunRunContext, snagTracker: SnagTracker): Future[RunResult] = {
          implicit val executionContext: ExecutionContextExecutor = context.materializer.executionContext
          fromWorker.getStreamBox(context, snagTracker).snagOrStream match {
            case Left(snag) =>
              Future {
                snagTracker.trackSnag(snag)
                RunResult(snagTracker.buildSeq())
              }
            case Right(headerAndRecordEtor) =>
              fileOpt match {
                case Some(file) =>
                  val writeChannelDisp = file.newWriteChannelDisposable(context.resourceConfig)
                  val channel = writeChannelDisp.a
                  val writer = new PrintWriter(Channels.newWriter(channel, StandardCharsets.UTF_8))
                  val doneFut = writeStreamAsTsv(headerAndRecordEtor, context, snagTracker)(writer.println)
                  doneFut.onComplete(_ => writer.close())
                  doneFut
                case None => writeStreamAsTsv(headerAndRecordEtor, context, snagTracker)(println)
              }
          }
        }

        override def getStream(context: LunRunContext, snagTracker: SnagTracker):
        Either[Snag, Source[String, RecordStreamWithMeta.Meta]] =
          fromWorker.getStreamBox(context,snagTracker).snagOrStream.map { recordStream =>
            getLineStream(recordStream)
          }
      })
    }
  }

}
