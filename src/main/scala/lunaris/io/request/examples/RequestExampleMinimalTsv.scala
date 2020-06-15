package lunaris.io.request.examples

import lunaris.io.request.Request
import lunaris.io.request.examples.RequestExamplesUtils.{PortalData, Regions, ToolCalls}
import lunaris.recipes.Recipe
import lunaris.recipes.values.LunValue.PrimitiveValue.{FileValue, StringValue}

object RequestExampleMinimalTsv extends RequestExample {
  override val idBase: String = "MinimalTsv"

  override def outputFile: FileValue = FileValue(s"response$idBase.tsv")

  val dataFile: FileValue = PortalData.Files.variants
  val idField: StringValue = PortalData.Fields.id

  object Keys {
    val read: String = "read"
    val write: String = "write"
  }

  override val request: Request =
    Request(id,
      Regions.simpleRegion,
      Recipe(Map(
        Keys.read -> ToolCalls.indexedObjectReader(dataFile, idField),
        Keys.write -> ToolCalls.tsvWriter(Keys.read, outputFile)
      ))
    )
}
