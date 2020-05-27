package lunaris.io.request.examples

import lunaris.genomics.Region
import lunaris.io.OutputId
import lunaris.io.request.examples.RequestExamplesUtils.PortalData
import lunaris.io.request.{Request, RequestJson}
import lunaris.recipes.Recipe
import lunaris.recipes.tools.ToolCall
import lunaris.recipes.tools.ToolCall.{RefArg, ValueArg}
import lunaris.recipes.tools.native.{IndexedObjectReader, JSONWriter}
import lunaris.recipes.values.LunValue.PrimitiveValue.{FileValue, StringValue}
import lunaris.utils.IOUtils

object RequestExampleMinimal extends RequestExample {
  override val id: String = "requestMinimal"

  object Keys {
    val read: String = "read"
    val write: String = "write"
  }

  override val request: Request =
    Request(id,
      Map("1" -> Seq(Region(100000, 200000))),
      Recipe(Map(
        Keys.read -> ToolCall(IndexedObjectReader, Map(
          IndexedObjectReader.Params.Keys.file ->
            ValueArg(IndexedObjectReader.Params.file, PortalData.variants),
          IndexedObjectReader.Params.Keys.idField -> ValueArg(IndexedObjectReader.Params.idField, StringValue("varId"))
        )
        ),
        Keys.write -> ToolCall(JSONWriter, Map(
          JSONWriter.Params.Keys.from -> RefArg(JSONWriter.Params.from, "read"),
          JSONWriter.Params.Keys.file ->
            ValueArg(JSONWriter.Params.file, FileValue("examples/responses/responseMinimal.json"))
        ))
      ))
    )
}
