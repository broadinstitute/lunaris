package lunaris.recipes.tools

import lunaris.recipes.tools.Tool.Param
import lunaris.recipes.values.LunValue.PrimitiveValue
import org.broadinstitute.yootilz.core.snag.Snag

case class ToolCall(tool: Tool, args: Map[String, ToolCall.Arg]) {
  def newInstance: Either[Snag, ToolInstance] = tool.newToolInstance(args)
}

object ToolCall {

  sealed trait Arg {
    def param: Tool.Param
  }

  case class ValueArg(param: Param, value: PrimitiveValue) extends Arg

  case class RefArg(param: Param, ref: String) extends Arg

  case class RefArrayArg(param: Param, refs: Seq[String]) extends Arg

}
