package lunaris.io.request.examples

import lunaris.io.request.Request
import lunaris.io.request.examples.RequestExamplesUtils.{PortalData, Regions, ToolCalls}
import lunaris.recipes.Recipe
import lunaris.recipes.values.LunValue.PrimitiveValue.{FileValue, StringValue}

object RequestExampleMinimalJson extends RequestExample {
  override val idBase: String = "MinimalJson"

  val dataFile: FileValue = PortalData.Files.variants
  val idField: StringValue = PortalData.Fields.varId

  object Keys {
    val read: String = "read"
    val write: String = "write"
  }

  override val request: Request =
    Request(id,
      Regions.simpleRegion,
      Recipe(Map(
        Keys.read -> ToolCalls.indexedObjectReader(dataFile, idField),
        Keys.write -> ToolCalls.jsonWriter(Keys.read, outputFile)
      ))
    )
}
