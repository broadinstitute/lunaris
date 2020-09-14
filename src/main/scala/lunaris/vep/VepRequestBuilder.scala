package lunaris.vep

import better.files.File
import lunaris.expressions.BooleanRecordExpression
import lunaris.genomics.{Region, Variant}
import lunaris.io.InputId
import lunaris.io.request.Request
import lunaris.io.request.examples.RequestExamplesUtils.ToolCalls
import lunaris.recipes.Recipe
import lunaris.recipes.tools.builtin.JoinRecordsWithFallback
import lunaris.recipes.values.LunType.StringType
import lunaris.recipes.values.LunValue.{ArrayValue, ExpressionValue}
import lunaris.recipes.values.LunValue.PrimitiveValue.{FileValue, StringValue}
import lunaris.vep.VepFileManager.ResultId

object VepRequestBuilder {

  def buildRequest(resultId: ResultId,
                   chroms: Seq[String],
                   regions: Map[String, Seq[Region]],
                   driverFileName: String,
                   dataFileName: String,
                   outputFile: File,
                   filter: BooleanRecordExpression,
                   indexFileNameOpt: Option[String],
                   varId: String): Request = {

    val requestId = "variant_effect_predictor_" + resultId.toString

    val fallbackString = "vep"

    val driverFile: FileValue = FileValue(driverFileName)
    val dataFile: FileValue = FileValue(dataFileName)
    val indexFileOpt: Option[FileValue] = indexFileNameOpt.map(FileValue)
    val idField: StringValue = StringValue(varId)
    val chromsValue: ArrayValue = ArrayValue(chroms.map(StringValue), StringType)
    val fallbackValue: StringValue = StringValue(fallbackString)
    val filterValue: ExpressionValue = ExpressionValue(filter)
    val outputFileValue: FileValue = FileValue(outputFile.toString())

    object Keys {
      val readDriver: String = "readDriver"
      val readData: String = "readData"
      val join: String = "join"
      val filter: String = "filter"
      val write: String = "write"
    }

    Request(requestId,
      regions,
      Recipe(Map(
        Keys.readDriver -> ToolCalls.vcfRecordsReader(driverFile, chromsValue),
        Keys.readData -> ToolCalls.indexedObjectReader(dataFile, indexFileOpt, idField),
        Keys.join -> ToolCalls.joinRecordsWithFallback(Keys.readDriver, Keys.readData, fallbackValue),
        Keys.filter -> ToolCalls.filter(Keys.join, filterValue),
        Keys.write -> ToolCalls.tsvWriter(Keys.filter, outputFileValue)
      ))
    )
  }

}
