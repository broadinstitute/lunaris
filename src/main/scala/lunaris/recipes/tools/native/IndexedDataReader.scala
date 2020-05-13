package lunaris.recipes.tools.native

import lunaris.io.InputId
import lunaris.recipes.eval.{LunRunContext, WorkerMaker}
import lunaris.recipes.tools.{Tool, ToolArgUtils, ToolCall}
import lunaris.recipes.values.LunType
import org.broadinstitute.yootilz.core.snag.Snag

object IndexedDataReader extends Tool {
  override def stage: Tool.Stage = Tool.Stage.Input

  override def resultType: LunType = LunType.RecordStreamType

  object Params {
    object Keys {
      val file = "file"
      val index= "index"
    }
    val file: Tool.ValueParam = Tool.ValueParam(Keys.file, LunType.FileType, isRequired = true)
    val index: Tool.ValueParam = Tool.ValueParam(Keys.index, LunType.FileType, isRequired = false)
  }

  override def params: Seq[Tool.Param] = Seq(Params.file, Params.index)

  override def newToolInstance(args: Map[String, ToolCall.Arg]): Either[Snag, ToolInstance] = {
    for {
      file <- ToolArgUtils.asInputId(Params.Keys.file, args)
      index <- ToolArgUtils.asInputIdOr(Params.Keys.index, args, file + ".tbi")
    } yield Instance(file, index)
  }

  case class Instance(file: InputId, index: InputId) extends ToolInstance {
    override def newWorkerMaker(context: LunRunContext): WorkerMaker = ???
  }
}
