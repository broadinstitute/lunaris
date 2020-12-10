package lunaris.vep

import better.files.File
import lunaris.app.VepDataFieldsSettings
import lunaris.expressions.LunBoolExpression
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
                   filter: LunBoolExpression,
                   indexFileNameOpt: Option[String],
                   dataFields: VepDataFieldsSettings): Request = {

    val requestId = "lunaris_vep_" + resultId.toString

    val fallbackString = "vep"

    val driverFile: FileValue = FileValue(driverFileName)
    val dataFile: FileValue = FileValue(dataFileName)
    val indexFileOpt: Option[FileValue] = indexFileNameOpt.map(FileValue)
    val idField: StringValue = StringValue(dataFields.varId)
    val refField: StringValue = StringValue(dataFields.ref)
    val altField: StringValue = StringValue(dataFields.alt)
    val refFieldVcf: StringValue = StringValue(VcfStreamVariantsReader.ColNames.ref)
    val altFieldVcf: StringValue = StringValue(VcfStreamVariantsReader.ColNames.alt)
    val idFieldNew: StringValue = StringValue("idCanon")
    val chromsValue: ArrayValue = ArrayValue(chroms.map(StringValue), StringType)
    val fallbackValue: StringValue = StringValue(fallbackString)
    val filterValue: ExpressionValue = ExpressionValue(filter)
    val outputFileValue: FileValue = FileValue(outputFile.toString())

    object Keys {
      val readDriver: String = "readDriver"
      val canonicalizeDriver: String = "canonicalizeDriver"
      val readData: String = "readData"
      val canonicalizeData: String = "canonicalizeData"
      val join: String = "join"
      val filter: String = "filter"
      val write: String = "write"
    }

    Request(requestId,
      regions,
      Recipe(Map(
        Keys.readDriver -> ToolCalls.vcfRecordsReader(driverFile, chromsValue),
        Keys.canonicalizeDriver -> ToolCalls.idCanonicalizer(Keys.readDriver, refFieldVcf, altFieldVcf, idFieldNew),
        Keys.readData -> ToolCalls.indexedObjectReader(dataFile, indexFileOpt, idField),
        Keys.canonicalizeData -> ToolCalls.idCanonicalizer(Keys.readData, refField, altField, idFieldNew),
        Keys.join -> ToolCalls.joinRecordsWithFallback(Keys.canonicalizeDriver, Keys.canonicalizeData, fallbackValue),
        Keys.filter -> ToolCalls.filter(Keys.join, filterValue),
        Keys.write -> ToolCalls.tsvWriter(Keys.filter, outputFileValue)
      ))
    )
  }

}
