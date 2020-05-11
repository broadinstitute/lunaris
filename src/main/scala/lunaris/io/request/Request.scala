package lunaris.io.request

import lunaris.genomics.Region
import lunaris.streams.tools.ToolCall

case class Request(id: String, regions: Map[String, Seq[Region]], tools: Map[String, ToolCall])
