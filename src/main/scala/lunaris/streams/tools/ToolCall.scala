package lunaris.streams.tools

trait ToolCall {
  def tool: Tool

  def args: Map[String, String]
}
