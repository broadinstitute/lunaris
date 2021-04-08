package lunaris.recipes.tools.builtin

import akka.stream.scaladsl.{Keep, Source}
import lunaris.io.OutputId
import lunaris.recipes.eval.LunRunnable.TextWriter
import lunaris.recipes.eval.LunWorker.RecordStreamWorker
import lunaris.recipes.eval.WorkerMaker.WorkerBox
import lunaris.recipes.eval.{LunCompileContext, LunWorker, RunTracker, WorkerMaker}
import lunaris.recipes.tools.{Tool, ToolArgUtils, ToolCall, ToolInstanceUtils}
import lunaris.recipes.values.{LunType, LunValue, LunValueJson, RecordStreamWithMeta}
import lunaris.recipes.{eval, tools}
import lunaris.vep.vcf.VcfCore
import lunaris.vep.vcf.VcfCore.VcfCoreRecord
import org.broadinstitute.yootilz.core.snag.Snag

object VcfRecordsWriter extends Tool {
  override def resultType: LunType.UnitType.type = LunType.UnitType
  object Params {

    object Keys {
      val from = "from"
      val file = "file"
      val refCol = "refCol"
      val altCol = "altCol"
    }

    val from: Tool.RefParam = Tool.RefParam(Keys.from, LunType.RecordStreamType, isRequired = true)
    val file: Tool.ValueParam = Tool.ValueParam(Keys.file, LunType.FileType, isRequired = false)
    val refCol: Tool.ValueParam = Tool.ValueParam(Keys.refCol, LunType.StringType, isRequired = true)
    val altCol: Tool.ValueParam = Tool.ValueParam(Keys.refCol, LunType.StringType, isRequired = true)
  }

  override def params: Seq[Tool.Param] = Seq(Params.from, Params.file, Params.refCol, Params.altCol)

  override def isFinal: Boolean = true

  override def newToolInstance(args: Map[String, ToolCall.Arg]): Either[Snag, tools.ToolInstance] = {
    for {
      from <- ToolArgUtils.asRef(Params.Keys.from, args)
      fileOpt <- ToolArgUtils.asOutputIdOpt(Params.Keys.file, args)
      refCol <- ToolArgUtils.asString(Params.Keys.refCol, args)
      altCol <- ToolArgUtils.asString(Params.Keys.altCol, args)
    } yield ToolInstance(from, fileOpt, refCol, altCol)
  }

  case class ToolInstance(from: String,
                          fileOpt: Option[OutputId],
                          refCol: String,
                          altCol: String) extends tools.ToolInstance {
    override def refs: Map[String, String] = Map(Params.Keys.from -> from)

    override def newWorkerMaker(context: LunCompileContext,
                                workers: Map[String, LunWorker]): Either[Snag, eval.WorkerMaker] = {
      ToolInstanceUtils.newWorkerMaker1(Params.Keys.from, workers) { fromWorker =>
        new WorkerMaker(fromWorker, fileOpt, refCol, altCol)
      }
    }
  }

  class WorkerMaker(fromWorker: RecordStreamWorker,
                    fileOpt: Option[OutputId],
                    refCol: String,
                    altCol: String) extends eval.WorkerMaker {
    override def nOrders: Int = 0

    override def orderAnotherWorker: Either[Snag, eval.WorkerMaker.Receipt] =
      Left(Snag(s"Tool $name is final and cannot be used as input for other tools."))

    override def finalizeAndShip(): WorkerBox = new WorkerBox {
      override def pickupWorkerOpt(receipt: WorkerMaker.Receipt): Option[LunWorker] = None

      private def getLineStream(recordStream: RecordStreamWithMeta, runTracker: RunTracker):
      Source[String, RecordStreamWithMeta.Meta] = {
        val vcfRecords = recordStream.source.mapConcat { record =>
          val snagOrVcfRecord = VcfCoreRecord.fromRecord(record, refCol, altCol)
          snagOrVcfRecord.left.foreach(runTracker.snagTracker.trackSnag)
          snagOrVcfRecord.toSeq
        }
        Source.single(VcfCore.ColNames.headerLine).concatMat(vcfRecords.map(_.toLine))(Keep.right)
      }

      override def pickupRunnableOpt(): Some[TextWriter] =
        Some[TextWriter](new TextWriter(fromWorker, fileOpt)(getLineStream))
    }
  }

}
