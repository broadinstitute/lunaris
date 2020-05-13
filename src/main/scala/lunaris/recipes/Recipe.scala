package lunaris.recipes

import lunaris.recipes.tools.ToolCall

case class Recipe(calls: Map[String, ToolCall])
