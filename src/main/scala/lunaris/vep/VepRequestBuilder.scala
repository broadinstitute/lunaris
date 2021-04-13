package lunaris.vep

import better.files.File
import lunaris.app.VepDataFieldsSettings
import lunaris.data.BlockGzippedWithIndex
import lunaris.expressions.LunBoolExpression
import lunaris.genomics.Region
import lunaris.io.request.Request
import lunaris.io.request.examples.RequestBuildUtils.ToolCalls
import lunaris.recipes.Recipe
import lunaris.recipes.values.LunType.StringType
import lunaris.recipes.values.LunValue.PrimitiveValue.{FileValue, StringValue}
import lunaris.recipes.values.LunValue.{ArrayValue, ExpressionValue}
import lunaris.vep.VepFileManager.JobId
import lunaris.vep.vcf.VcfCore

case class VepRequestBuilder(resultId: JobId,
                             chroms: Seq[String],
                             regions: Map[String, Seq[Region]],
                             driverFileName: String) {
  val chromsValue: ArrayValue = ArrayValue(chroms.map(StringValue), StringType)
  val driverFile: FileValue = FileValue(driverFileName)

  def buildPhaseOneRequest(): Request = {
    object Keys {
      val readDriver: String = "readDriver"
      val restrictToExome: String = "restrictToExome"
    }
    val requestId = "lunaris_vep_phase_one_" + resultId.toString
    val toolCalls = Map(
      Keys.readDriver -> ToolCalls.vcfRecordsReader(driverFile, chromsValue)
    )
    Request(requestId, regions, Recipe(toolCalls))
  }

  def buildPhaseTwoRequest(): Request = {
    val requestId = "lunaris_vep_phase_two_" + resultId.toString
    val toolCalls = ???
    Request(requestId, regions, Recipe(toolCalls))
  }
}

object VepRequestBuilder {

  def buildRequestOld(resultId: JobId,
                      chroms: Seq[String],
                      regions: Map[String, Seq[Region]],
                      driverFileName: String,
                      dataFilesWithIndices: Seq[BlockGzippedWithIndex],
                      outputFile: File,
                      outFileFormat: String,
                      filter: LunBoolExpression,
                      dataFields: VepDataFieldsSettings): Request = {

    val requestId = "lunaris_vep_" + resultId.toString

    val fallbackString = "vep"

    val driverFile: FileValue = FileValue(driverFileName)
    val idField: StringValue = StringValue(dataFields.varId)
    val refField: StringValue = StringValue(dataFields.ref)
    val altField: StringValue = StringValue(dataFields.alt)
    val refFieldVcf: StringValue = StringValue(VcfCore.ColNames.ref)
    val altFieldVcf: StringValue = StringValue(VcfCore.ColNames.alt)
    val idFieldNew: StringValue = StringValue("idCanon")
    val chromsValue: ArrayValue = ArrayValue(chroms.map(StringValue), StringType)
    val fallbackValue: StringValue = StringValue(fallbackString)
    val filterValue: ExpressionValue = ExpressionValue(filter)
    val outputFileValue: FileValue = FileValue(outputFile.toString())
    val outputFileFormatValue: StringValue = StringValue(outFileFormat)

    object Keys {
      val readDriver: String = "readDriver"
      val canonicalizeDriver: String = "canonicalizeDriver"

      def readData(i: Int): String = s"readData$i"

      def canonicalizeData(i: Int): String = s"canonicalizeData$i"

      val join: String = "join"
      val calculateMaf: String = "calculateMaf"
      val filter: String = "filter"
      val write: String = "write"
    }
    val dataToolCalls = dataFilesWithIndices.zipWithIndex.collect {
      case (dataFileWithIndex, i) =>
        val dataFileValue = FileValue(dataFileWithIndex.data.toString)
        val indexFileValue = FileValue(dataFileWithIndex.index.toString)
        Map(
          Keys.readData(i) -> ToolCalls.indexedObjectReader(dataFileValue, Some(indexFileValue), idField),
          Keys.canonicalizeData(i) -> ToolCalls.idCanonicalizer(Keys.readData(i), refField, altField, idFieldNew),
        )
    }.fold(Map.empty)(_ ++ _)
    val indices = dataFilesWithIndices.indices
    val otherToolCalls = Map(
      Keys.readDriver -> ToolCalls.vcfRecordsReader(driverFile, chromsValue),
      Keys.canonicalizeDriver -> ToolCalls.idCanonicalizer(Keys.readDriver, refFieldVcf, altFieldVcf, idFieldNew),
      Keys.join ->
        ToolCalls.joinRecordsWithFallback(Keys.canonicalizeDriver, indices.map(Keys.canonicalizeData), fallbackValue),
      Keys.calculateMaf -> ToolCalls.calculateMaf(Keys.join),
      Keys.filter -> ToolCalls.filter(Keys.calculateMaf, filterValue),
      Keys.write -> ToolCalls.groupFileWriter(Keys.filter, outputFileValue, outputFileFormatValue)
    )
    val toolCalls = dataToolCalls ++ otherToolCalls
    Request(requestId, regions, Recipe(toolCalls))
  }
}
