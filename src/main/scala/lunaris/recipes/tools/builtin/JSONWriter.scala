package lunaris.recipes.tools.builtin

import akka.stream.scaladsl.{Keep, Source}
import lunaris.io.OutputId
import lunaris.recipes.eval.LunRunnable.TextWriter
import lunaris.recipes.eval.LunWorker.RecordStreamWorker
import lunaris.recipes.eval.WorkerMaker.WorkerBox
import lunaris.recipes.eval._
import lunaris.recipes.tools.{Tool, ToolArgUtils, ToolCall, ToolInstanceUtils}
import lunaris.recipes.values.{LunType, LunValueJson, RecordStreamWithMeta}
import lunaris.recipes.{eval, tools}
import org.broadinstitute.yootilz.core.snag.Snag

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

      private def toLineSource(recordSource: RecordStreamWithMeta):
      Source[String, RecordStreamWithMeta.Meta] = {
        val recordJsonStringStream = recordSource.source.map(Some(_))
          .concat(Source(Seq(None)))
          .sliding(2)
          .map { recordOpts =>
            val isLast = recordOpts.size < 2 || recordOpts(1).isEmpty
            val record = recordOpts.head.get
            val objectJsonString = LunValueJson.toJson(record)
            val idWithJson =
              "  \"" + record.id + "\" : " + objectJsonString.replace("\n", "\n  ")
            val maybeComma = if (isLast) "" else ","
            idWithJson + maybeComma
          }
        Source.single("{\n")
          .concatMat(recordJsonStringStream)(Keep.right)
          .concat(Source.single("}\n"))
      }

      override def pickupRunnableOpt(): Some[TextWriter] =
        Some[TextWriter](new TextWriter(fromWorker, fileOpt)(toLineSource))
    }
  }

}
