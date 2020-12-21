package lunaris.io.request.examples

import lunaris.io.request.Request
import lunaris.io.request.examples.RequestBuildUtils.{PortalData, Regions, ToolCalls}
import lunaris.recipes.Recipe
import lunaris.recipes.values.LunValue.PrimitiveValue.{FileValue, StringValue}

object RequestExampleSimpleFilter extends RequestExample {
  override val idBase: String = "SimpleFilterTsv"

  override def outputFile: FileValue = FileValue(s"response$idBase.tsv")

  val dataFile: FileValue = PortalData.Files.associations
  val varId: StringValue = PortalData.Fields.varId
  val phenotype: StringValue = PortalData.Fields.phenotype
  val stringValue: StringValue = StringValue("T2D")

  object Keys {
    val read: String = "read"
    val filter: String = "filter"
    val write: String = "write"
  }

  override val request: Request =
    Request(id,
      Regions.somewhatBiggerRegion,
      Recipe(Map(
        Keys.read -> ToolCalls.indexedObjectReader(dataFile, None, varId),
        Keys.filter -> ToolCalls.simpleFilter(Keys.read, phenotype, stringValue),
        Keys.write -> ToolCalls.tsvWriter(Keys.filter, outputFile)
      ))
    )
}
