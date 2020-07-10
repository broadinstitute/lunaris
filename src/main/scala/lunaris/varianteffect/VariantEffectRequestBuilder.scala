package lunaris.varianteffect

import better.files.File
import lunaris.genomics.Variant
import lunaris.io.request.Request
import lunaris.io.request.examples.RequestExamplesUtils.{PortalData, ToolCalls}
import lunaris.recipes.Recipe
import lunaris.recipes.values.LunValue.PrimitiveValue.{FileValue, StringValue}
import lunaris.varianteffect.ResultFileManager.ResultId

object VariantEffectRequestBuilder {

  def buildRequest(resultId: ResultId,
                   variantsByChrom: Map[String, Seq[Variant]],
                   outputFile: File): Request = {

    val requestId = "variant_effect_predictor_" + resultId.toString

    val regions = variantsByChrom.view.mapValues(_.map(_.toLocus.region)).toMap

    val dataFile: FileValue = PortalData.Files.variants
    val idField: StringValue = PortalData.Fields.varId
    val outputFileValue: FileValue = FileValue(outputFile.toString())

    object Keys {
      val read: String = "read"
      val write: String = "write"
    }

    Request(requestId,
      regions,
      Recipe(Map(
        Keys.read -> ToolCalls.indexedObjectReader(dataFile, idField),
        Keys.write -> ToolCalls.tsvWriter(Keys.read, outputFileValue)
      ))
    )
  }

}
