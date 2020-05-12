package lunaris.streams.tools.native

import lunaris.streams.tools.Tool
import lunaris.streams.values.LunType

object DataFileReader extends Tool {
  override def name: String = "DataFileReader"

  override def resultType: LunType = LunType.RecordStreamType

  object Params {
    val file: Tool.ValueParam = Tool.ValueParam("file", LunType.FileType, isRequired = true)
    val index: Tool.RefParam = Tool.RefParam("index", LunType.FileType, isRequired = false)
  }

  override def params: Seq[Tool.Param] = Seq(Params.file, Params.index)
}
