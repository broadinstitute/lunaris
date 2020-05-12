package lunaris.streams.tools.native

import lunaris.streams.tools.Tool
import lunaris.streams.values.LunType

object IndexedDataReader extends Tool {
  override def stage: Tool.Stage = Tool.Stage.Input

  override def resultType: LunType = LunType.RecordStreamType

  object Params {
    val file: Tool.ValueParam = Tool.ValueParam("file", LunType.FileType, isRequired = true)
    val index: Tool.ValueParam = Tool.ValueParam("index", LunType.FileType, isRequired = false)
  }

  override def params: Seq[Tool.Param] = Seq(Params.file, Params.index)
}
