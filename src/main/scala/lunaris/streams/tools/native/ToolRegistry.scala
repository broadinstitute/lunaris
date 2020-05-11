package lunaris.streams.tools.native

import lunaris.streams.tools.Tool

object ToolRegistry {
  val tools: Set[Tool] = Set(DataFileReader)
  val toolsByName: Map[String, Tool] = tools.map(tool => (tool.name, tool)).toMap
}
