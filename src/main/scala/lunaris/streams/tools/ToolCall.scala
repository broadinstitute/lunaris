package lunaris.streams.tools

import lunaris.streams.tools.Tool.Param
import lunaris.streams.values.LunPrimitiveValue

case class ToolCall(tool: Tool, args: Map[String, ToolCall.Arg])

object ToolCall {

  sealed trait Arg {
    def param: Tool.Param
  }

  case class ValueArg(param: Param, value: LunPrimitiveValue) extends Arg

  case class RefArg(param: Param, ref: String) extends Arg

}
