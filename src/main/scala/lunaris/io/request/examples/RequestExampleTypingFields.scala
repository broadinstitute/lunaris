package lunaris.io.request.examples

import lunaris.io.request.Request
import lunaris.io.request.examples.RequestExamplesUtils.{PortalData, Regions, ToolCalls}
import lunaris.recipes.Recipe
import lunaris.recipes.values.{LunType, LunValue}
import lunaris.recipes.values.LunValue.MapValue
import lunaris.recipes.values.LunValue.PrimitiveValue.{FileValue, StringValue}

object RequestExampleTypingFields extends RequestExample {
  override val idBase: String = "TypingFields"

  val dataFile: FileValue = PortalData.Files.variants
  val idField: StringValue = PortalData.Fields.varId
  val types: MapValue = MapValue(Map("maf" -> LunValue.TypeValue(LunType.FloatType)), LunType.TypeType)

  object Keys {
    val read: String = "read"
    val write: String = "write"
  }

  override val request: Request =
    Request(id,
      Regions.simpleRegion,
      Recipe(Map(
        Keys.read -> ToolCalls.indexedObjectReader(dataFile, None, idField, Some(types)),
        Keys.write -> ToolCalls.jsonWriter(Keys.read, outputFile)
      ))
    )
}
