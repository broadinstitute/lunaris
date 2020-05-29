package lunaris.recipes.tools.native

import lunaris.recipes.tools.Tool

object ToolRegistry {
  val tools: Set[Tool] = Set(IndexedObjectReader, TSVWriter, JSONWriter)
  val toolsByName: Map[String, Tool] = tools.map(tool => (tool.name, tool)).toMap
}
