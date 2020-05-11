package lunaris.streams.tools.native

import lunaris.streams.tools.Tool
import lunaris.streams.values.LunType

object DataFileReader extends Tool {
  override def name: String = "DataFileReader"

  override def resultType: LunType = LunType.RecordStreamType

  override def params: Seq[Tool.Param] =
    Seq(
      Tool.Param("file", LunType.FileType, isRef = false, isRequired = true),
        Tool.Param("index", LunType.FileType, isRef = false, isRequired = false)
    )
}
