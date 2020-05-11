package lunaris.streams.tools

import lunaris.streams.values.LunPrimitiveValue

trait ToolCall {
  def tool: Tool

  def args: Map[String, ToolCall.Arg]
}

object ToolCall {
  sealed trait Arg
  case class ValueArg(value: LunPrimitiveValue) extends Arg
  case class RefArg(name: String) extends Arg
}
