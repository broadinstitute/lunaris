package lunaris.recipes.tools.builtin

import lunaris.recipes.tools.Tool

object ToolRegistry {
  val tools: Set[Tool] = Set(IndexedRecordReader, TSVWriter, JSONWriter, RecordsSimpleFilter, RecordsFilter)
  val toolsByName: Map[String, Tool] = tools.map(tool => (tool.name, tool)).toMap
}
