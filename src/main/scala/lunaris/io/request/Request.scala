package lunaris.io.request

import lunaris.genomics.Region
import lunaris.recipes.Recipe
import lunaris.recipes.tools.ToolCall

case class Request(id: String, regions: Map[String, Seq[Region]], recipe: Recipe)
