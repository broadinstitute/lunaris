package lunaris.varianteffect

import better.files.File
import lunaris.expressions.BooleanRecordExpression
import lunaris.genomics.Variant
import lunaris.io.request.Request
import lunaris.io.request.examples.RequestExamplesUtils.ToolCalls
import lunaris.recipes.Recipe
import lunaris.recipes.values.LunValue.ExpressionValue
import lunaris.recipes.values.LunValue.PrimitiveValue.{FileValue, StringValue}
import lunaris.varianteffect.ResultFileManager.ResultId

object VariantEffectRequestBuilder {

  def buildRequest(resultId: ResultId,
                   variantsByChrom: Map[String, Seq[Variant]],
                   outputFile: File,
                   dataFileName: String,
                   filter: BooleanRecordExpression,
                   indexFileNameOpt: Option[String],
                   varId: String): Request = {

    val requestId = "variant_effect_predictor_" + resultId.toString

    val regions = variantsByChrom.view.mapValues(_.map(_.toLocus.region)).toMap

    println("yo" + regions.head._2.take(10).mkString(", "))

    val dataFile: FileValue = FileValue(dataFileName)
    val indexFileOpt: Option[FileValue] = indexFileNameOpt.map(FileValue)
    val idField: StringValue = StringValue(varId)
    val filterValue: ExpressionValue = ExpressionValue(filter)
    val outputFileValue: FileValue = FileValue(outputFile.toString())

    object Keys {
      val read: String = "read"
      val filter: String = "filter"
      val write: String = "write"
    }

    Request(requestId,
      regions,
      Recipe(Map(
        Keys.read -> ToolCalls.indexedObjectReader(dataFile, indexFileOpt, idField),
        Keys.filter -> ToolCalls.filter(Keys.read, filterValue),
        Keys.write -> ToolCalls.tsvWriter(Keys.filter, outputFileValue)
      ))
    )
  }

}
