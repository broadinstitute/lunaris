package lunaris.varianteffect

import better.files.File
import lunaris.genomics.Variant
import lunaris.io.request.Request
import lunaris.io.request.examples.RequestExamplesUtils.ToolCalls
import lunaris.recipes.Recipe
import lunaris.recipes.values.LunValue.PrimitiveValue.{FileValue, StringValue}
import lunaris.varianteffect.ResultFileManager.ResultId

object VariantEffectRequestBuilder {

  def buildRequest(resultId: ResultId,
                   variantsByChrom: Map[String, Seq[Variant]],
                   outputFile: File,
                   dataFileName: String,
                   indexFileNameOpt: Option[String],
                   varId: String): Request = {

    val requestId = "variant_effect_predictor_" + resultId.toString

    val regions = variantsByChrom.view.mapValues(_.map(_.toLocus.region)).toMap

    val dataFile: FileValue = FileValue(dataFileName)
    val indexFileOpt: Option[FileValue] = indexFileNameOpt.map(FileValue)
    val idField: StringValue = StringValue(varId)
    val outputFileValue: FileValue = FileValue(outputFile.toString())

    object Keys {
      val read: String = "read"
      val write: String = "write"
    }

    Request(requestId,
      regions,
      Recipe(Map(
        Keys.read -> ToolCalls.indexedObjectReader(dataFile, indexFileOpt, idField),
        Keys.write -> ToolCalls.tsvWriter(Keys.read, outputFileValue)
      ))
    )
  }

}
