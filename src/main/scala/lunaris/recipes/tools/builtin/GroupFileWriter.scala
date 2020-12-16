package lunaris.recipes.tools.builtin

import akka.stream.scaladsl.{Keep, Source}
import lunaris.io.OutputId
import lunaris.recipes.eval.LunRunnable.TextWriter
import lunaris.recipes.eval.LunWorker.RecordStreamWorker
import lunaris.recipes.eval.WorkerMaker.WorkerBox
import lunaris.recipes.eval.{LunCompileContext, LunWorker, WorkerMaker}
import lunaris.recipes.tools.{Tool, ToolArgUtils, ToolCall, ToolInstanceUtils}
import lunaris.recipes.values.{LunType, RecordStreamWithMeta}
import lunaris.recipes.{eval, tools}
import lunaris.streams.transform.RareMetalsGroupSerializer
import lunaris.streams.utils.RecordStreamTypes.Record
import org.broadinstitute.yootilz.core.snag.Snag

object GroupFileWriter extends Tool {
  override def resultType: LunType.UnitType.type = LunType.UnitType

  object Params {

    object Keys {
      val from = "from"
      val file = "file"
      val format = "format"
    }

    val from: Tool.RefParam = Tool.RefParam(Keys.from, LunType.RecordStreamType, isRequired = true)
    val file: Tool.ValueParam = Tool.ValueParam(Keys.file, LunType.FileType, isRequired = false)
    val format: Tool.ValueParam = Tool.ValueParam(Keys.format, LunType.StringType, isRequired = true)
  }

  override def params: Seq[Tool.Param] = Seq(Params.from, Params.file, Params.format)

  override def isFinal: Boolean = true

  override def newToolInstance(args: Map[String, ToolCall.Arg]): Either[Snag, tools.ToolInstance] = {
    for {
      from <- ToolArgUtils.asRef(Params.Keys.from, args)
      fileOpt <- ToolArgUtils.asOutputIdOpt(Params.Keys.file, args)
      format <- ToolArgUtils.asString(Params.Keys.format, args)
    } yield ToolInstance(from, fileOpt, format)
  }

  case class ToolInstance(from: String,
                          fileOpt: Option[OutputId],
                          format: String) extends tools.ToolInstance {
    override def refs: Map[String, String] = Map(Params.Keys.from -> from)

    override def newWorkerMaker(context: LunCompileContext,
                                workers: Map[String, LunWorker]): Either[Snag, eval.WorkerMaker] = {
      ToolInstanceUtils.newWorkerMaker1(Params.Keys.from, workers) { fromWorker =>
        new WorkerMaker(fromWorker, fileOpt, format)
      }
    }
  }
  class WorkerMaker(fromWorker: RecordStreamWorker,
                    fileOpt: Option[OutputId], format: String) extends eval.WorkerMaker {
    override def nOrders: Int = 0

    override def orderAnotherWorker: Either[Snag, eval.WorkerMaker.Receipt] =
      Left(Snag(s"Tool $name is final and cannot be used as input for other tools."))

    override def finalizeAndShip(): WorkerBox = new WorkerBox {
      override def pickupWorkerOpt(receipt: WorkerMaker.Receipt): Option[LunWorker] = None

      override def pickupRunnableOpt(): Some[TextWriter] = {
        val groupIdFields = Seq("SYMBOL", "Gene")
        val groupSerializer = new RareMetalsGroupSerializer(groupIdFields)
        Some(new TextWriter(fromWorker, fileOpt)(groupSerializer.recordsToRareMetalsLines))
      }
    }
  }
}
