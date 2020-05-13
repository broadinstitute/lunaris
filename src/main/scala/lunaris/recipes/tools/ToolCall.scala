package lunaris.recipes.tools

import lunaris.recipes.tools.Tool.Param
import lunaris.recipes.values.LunPrimitiveValue

case class ToolCall(tool: Tool, args: Map[String, ToolCall.Arg])

object ToolCall {

  sealed trait Arg {
    def param: Tool.Param
  }

  case class ValueArg(param: Param, value: LunPrimitiveValue) extends Arg

  case class RefArg(param: Param, ref: String) extends Arg

}
