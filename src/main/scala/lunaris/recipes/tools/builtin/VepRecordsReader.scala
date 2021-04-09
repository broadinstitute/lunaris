package lunaris.recipes.tools.builtin

import lunaris.io.InputId
import lunaris.recipes.eval.LunWorker.RecordStreamWorker
import lunaris.recipes.eval.{LunCompileContext, LunRunContext, LunRunnable, LunWorker, RunTracker, WorkerMaker}
import lunaris.recipes.eval.WorkerMaker.WorkerBox
import lunaris.recipes.{eval, tools}
import lunaris.recipes.tools.{Tool, ToolArgUtils, ToolCall}
import lunaris.recipes.values.{LunType, RecordStreamWithMeta}
import lunaris.recipes.values.RecordStreamWithMeta.Meta
import lunaris.vep.VepOutputReader
import org.broadinstitute.yootilz.core.snag.Snag

object VepRecordsReader extends tools.Tool {
  override def resultType: LunType = LunType.RecordStreamType

  object Params {

    object Keys {
      val file = "file"
      val chroms = "chroms"
    }

    val file: Tool.ValueParam = Tool.ValueParam(Keys.file, LunType.FileType, isRequired = true)
    val chroms: Tool.ValueParam =
      Tool.ValueParam(Keys.chroms, LunType.ArrayType(LunType.StringType), isRequired = true)
  }

  override def params: Seq[Tool.Param] = Seq(Params.file, Params.chroms)

  override def isFinal: Boolean = false

  override def newToolInstance(args: Map[String, ToolCall.Arg]): Either[Snag, ToolInstance] = {
    for {
      file <- ToolArgUtils.asInputId(Params.Keys.file, args)
      chroms <- ToolArgUtils.asStrings(Params.Keys.chroms, args)
    } yield ToolInstance(file, chroms)
  }

  case class ToolInstance(file: InputId, chroms: Seq[String]) extends tools.ToolInstance {
    override def refs: Map[String, String] = Map.empty

    override def newWorkerMaker(context: LunCompileContext,
                                workers: Map[String, LunWorker]): Either[Snag, eval.WorkerMaker] =
      Right(new WorkerMaker(file, chroms))
  }

  class WorkerMaker(file: InputId,
                    chroms: Seq[String]) extends eval.WorkerMaker with eval.WorkerMaker.WithOutput {
    override def finalizeAndShip(): WorkerMaker.WorkerBox = new WorkerBox {
      override def pickupWorkerOpt(receipt: WorkerMaker.Receipt): Option[RecordStreamWorker] =
        Some[RecordStreamWorker] {
          (context: LunRunContext, runTracker: RunTracker) => {
//            VepOutputReader.read()
//            val recordType = VcfCore.vcfRecordType
//            val meta = Meta(recordType, chroms)
//            val source = VcfStreamVariantsReader.readVcfRecords(file.newStream(context.resourceConfig))
//              .map(_.toRecord).mapMaterializedValue(_ => meta)
//            val stream = RecordStreamWithMeta(meta, source)
//            LunWorker.StreamBox(stream)
            ???  //  TODO
          }
        }

      override def pickupRunnableOpt(): Option[LunRunnable] = None
    }
  }


}
