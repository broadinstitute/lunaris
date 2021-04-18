package lunaris.vep

import better.files.File
import lunaris.app.VepDataFieldsSettings
import lunaris.data.BlockGzippedWithIndex
import lunaris.expressions.LunBoolExpression
import lunaris.genomics.Region
import lunaris.io.request.Request
import lunaris.io.request.examples.RequestBuildUtils.ToolCalls
import lunaris.recipes.Recipe
import lunaris.recipes.tools.ToolCall
import lunaris.recipes.tools.builtin.JoinRecordsWithFallback.FallBacks
import lunaris.recipes.values.LunType.StringType
import lunaris.recipes.values.LunValue.PrimitiveValue.{FileValue, StringValue}
import lunaris.recipes.values.LunValue.{ArrayValue, ExpressionValue}
import lunaris.vep.VepJobManager.JobId
import lunaris.vep.vcf.VcfCore

case class VepRequestBuilder(jobId: JobId,
                             jobFiles: VepJobFiles,
                             chroms: Seq[String],
                             regions: Map[String, Seq[Region]],
                             exomeFileName: File,
                             dataFields: VepDataFieldsSettings,
                             dataFilesWithIndices: Seq[BlockGzippedWithIndex],
                             filter: LunBoolExpression,
                             outFileFormat: String) {
  val chromsValue: ArrayValue = ArrayValue(chroms.map(StringValue), StringType)
  val driverFile: FileValue = FileValue(jobFiles.inputFile)
  val exomesFile: FileValue = FileValue(exomeFileName)
  val refFieldVcf: StringValue = StringValue(VcfCore.ColNames.ref)
  val altFieldVcf: StringValue = StringValue(VcfCore.ColNames.alt)
  val idFieldVep: StringValue = StringValue(dataFields.varId)
  val refFieldVep: StringValue = StringValue(dataFields.ref)
  val altFieldVep: StringValue = StringValue(dataFields.alt)
  val idFieldNew: StringValue = StringValue("idCanon")
  val cacheMissesFile: FileValue = FileValue(jobFiles.vepInputFile)
  val cacheFile: FileValue = FileValue(jobFiles.vepOutputFile)
  val filterValue: ExpressionValue = ExpressionValue(filter)
  val outputFileValue: FileValue = FileValue(jobFiles.outputFile)
  val outputFileFormatValue: StringValue = StringValue(outFileFormat)
  val idFallback: StringValue = StringValue(FallBacks.id)

  object Keys {
    val readDriver: String = "readDriver"
    val restrictToExome: String = "restrictToExome"
    val canonicalizeDriver: String = "canonicalizeDriver"
    val readDatas: Seq[String] = dataFilesWithIndices.indices.map("readData" + _)
    val canonicalizeDatas: Seq[String] = dataFilesWithIndices.indices.map("canonicalizeData" + _)
    val cacheMisses: String = "cacheMisses"
    val writeCacheMisses: String = "writeCacheMisses"
    val readCache: String = "readCache"
    val pickFromCache: String = "pickFromCache"
    val join: String = "join"
    val calculateMaf: String = "calculateMaf"
    val filter: String = "filter"
    val write: String = "write"
  }

  private def dataToolCalls(): Map[String, ToolCall] = {
    dataFilesWithIndices.zip(Keys.readDatas).zip(Keys.canonicalizeDatas).collect {
      case ((dataFileWithIndex, readDataKey), canonicalizeDataKey) =>
        val dataFileValue = FileValue(dataFileWithIndex.data.toString)
        val indexFileValue = FileValue(dataFileWithIndex.index.toString)
        Map(
          readDataKey -> ToolCalls.indexedObjectReader(dataFileValue, Some(indexFileValue), idFieldVep),
          canonicalizeDataKey -> ToolCalls.idCanonicalizer(readDataKey, refFieldVep, altFieldVep, idFieldNew)
        )
    }.fold(Map.empty)(_ ++ _)
  }

  private def initialToolCalls(): Map[String, ToolCall] = {
    Map(
      Keys.readDriver -> ToolCalls.vcfRecordsReader(driverFile, chromsValue),
      Keys.restrictToExome -> ToolCalls.restrictToRegions(Keys.readDriver, exomesFile),
      Keys.canonicalizeDriver -> ToolCalls.idCanonicalizer(Keys.restrictToExome, refFieldVcf, altFieldVcf, idFieldNew)
    ) ++ dataToolCalls()
  }

  def buildPhaseOneRequest(): Request = {
    val requestId = "lunaris_vep_phase_one_" + jobId.toString
    val toolCalls = {
      initialToolCalls() ++ Map(
        Keys.cacheMisses -> ToolCalls.findRecordsNotInData(Keys.canonicalizeDriver, Keys.canonicalizeDatas),
        Keys.writeCacheMisses ->
          ToolCalls.vcfRecordsWriter(Keys.cacheMisses, cacheMissesFile, refFieldVcf, altFieldVcf)
      )
    }
    Request(requestId, regions, Recipe(toolCalls))
  }

  def buildPhaseTwoRequest(): Request = {
    val requestId = "lunaris_vep_phase_two_" + jobId.toString
    val toolCalls =
      initialToolCalls() ++ Map(
        Keys.readCache -> ToolCalls.vepRecordsReader(cacheFile, chromsValue),
        Keys.join ->
          ToolCalls.joinRecordsWithFallback(
            Keys.canonicalizeDriver, Keys.canonicalizeDatas :+ Keys.readCache, idFallback
          ),
        Keys.calculateMaf -> ToolCalls.calculateMaf(Keys.join),
        Keys.filter -> ToolCalls.filter(Keys.calculateMaf, filterValue),
        Keys.write -> ToolCalls.groupFileWriter(Keys.filter, outputFileValue, outputFileFormatValue)
      )
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
