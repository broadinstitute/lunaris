package lunaris.recipes.tools.builtin

import lunaris.io.InputId
import lunaris.recipes.eval.{LunCompileContext, LunWorker, WorkerMaker}
import lunaris.recipes.tools.{Tool, ToolArgUtils, ToolCall}
import lunaris.recipes.tools.builtin.IndexedRecordReader.{Params, ToolInstance, WorkerMaker}
import lunaris.recipes.values.{LunType, LunValue}
import lunaris.recipes.{eval, tools}
import lunaris.streams.RecordProcessor
import org.broadinstitute.yootilz.core.snag.Snag

object VcfRecordsReader extends tools.Tool {
  override def resultType: LunType = LunType.RecordStreamType

  object Params {

    object Keys {
      val file = "file"
    }

    val file: Tool.ValueParam = Tool.ValueParam(Keys.file, LunType.FileType, isRequired = true)
  }

  override def params: Seq[Tool.Param] = Seq(Params.file)

  override def isFinal: Boolean = false

  override def newToolInstance(args: Map[String, ToolCall.Arg]): Either[Snag, ToolInstance] = {
    for {
      file <- ToolArgUtils.asInputId(Params.Keys.file, args)
    } yield ToolInstance(file)
  }

  case class ToolInstance(file: InputId) extends tools.ToolInstance {
    override def refs: Map[String, String] = Map.empty

    override def newWorkerMaker(context: LunCompileContext,
                                workers: Map[String, LunWorker]): Either[Snag, eval.WorkerMaker] =
      Right(new WorkerMaker(file, RecordProcessor.ignoreFaultyRecords, context))

  }

  class WorkerMaker(file: InputId,
                    recordProcessor: RecordProcessor[LunValue.RecordValue],
                    compileContext: LunCompileContext) extends eval.WorkerMaker with eval.WorkerMaker.WithOutput {
    override def finalizeAndShip(): WorkerMaker.WorkerBox = ???
  }

}
