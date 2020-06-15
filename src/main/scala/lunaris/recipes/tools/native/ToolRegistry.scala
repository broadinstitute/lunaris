package lunaris.recipes.tools.native

import lunaris.recipes.tools.Tool

object ToolRegistry {
  val tools: Set[Tool] = Set(IndexedRecordReader, TSVWriter, JSONWriter, RecordsFilter)
  val toolsByName: Map[String, Tool] = tools.map(tool => (tool.name, tool)).toMap
}
