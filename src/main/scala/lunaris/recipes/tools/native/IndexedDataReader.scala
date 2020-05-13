package lunaris.recipes.tools.native

import lunaris.recipes.tools.{Tool, ToolCall}
import lunaris.recipes.values.LunType
import org.broadinstitute.yootilz.core.snag.Snag

object IndexedDataReader extends Tool {
  override def stage: Tool.Stage = Tool.Stage.Input

  override def resultType: LunType = LunType.RecordStreamType

  object Params {
    val file: Tool.ValueParam = Tool.ValueParam("file", LunType.FileType, isRequired = true)
    val index: Tool.ValueParam = Tool.ValueParam("index", LunType.FileType, isRequired = false)
  }

  override def params: Seq[Tool.Param] = Seq(Params.file, Params.index)

  override def newToolInstance(args: Map[String, ToolCall.Arg]): Either[Snag, ToolInstance] = ???
}
