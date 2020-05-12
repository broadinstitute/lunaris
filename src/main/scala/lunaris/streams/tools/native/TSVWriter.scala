package lunaris.streams.tools.native

import lunaris.streams.tools.Tool
import lunaris.streams.values.LunType

object TSVWriter extends Tool {
  override def stage: Tool.Stage = Tool.Stage.Output

  override def resultType: LunType.UnitType.type = LunType.UnitType

  object Params {
    val from: Tool.RefParam = Tool.RefParam("from", LunType.RecordStreamType, isRequired = true)
    val file: Tool.ValueParam = Tool.ValueParam("file", LunType.StringType, isRequired = false)
  }

  override def params: Seq[Tool.Param] = Seq(Params.from, Params.file)
}
