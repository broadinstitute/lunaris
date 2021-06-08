package lunaris.vep

import better.files.File
import lunaris.app.VepDataFieldsSettings
import lunaris.expressions.LunBoolExpression
import lunaris.genomics.Region
import lunaris.io.request.Request
import lunaris.io.request.examples.RequestBuildUtils.ToolCalls
import lunaris.recipes.Recipe
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
                             filter: LunBoolExpression,
                             outFileFormat: String) {
  val chromsValue: ArrayValue = ArrayValue(chroms.map(StringValue), StringType)
  val driverFile: FileValue = FileValue(jobFiles.inputFile)
  val exomesFile: FileValue = FileValue(exomeFileName)
  val refFieldVcf: StringValue = StringValue(VcfCore.ColNames.ref)
  val altFieldVcf: StringValue = StringValue(VcfCore.ColNames.alt)
  val refFieldVep: StringValue = StringValue(dataFields.ref)
  val altFieldVep: StringValue = StringValue(dataFields.alt)
  val idFieldNew: StringValue = StringValue("idCanon")
  val extractedDataFile: FileValue = FileValue(jobFiles.extractedDataFile)
  val cacheMissesFile: FileValue = FileValue(jobFiles.cacheMissesFile)
  val vepInputFile: FileValue = FileValue(jobFiles.vepInputFile)
  val vepOutputFile: FileValue = FileValue(jobFiles.vepOutputFile)
  val filterValue: ExpressionValue = ExpressionValue(filter)
  val outputFileValue: FileValue = FileValue(jobFiles.outputFile)
  val outputFileFormatValue: StringValue = StringValue(outFileFormat)
  val idFallback: StringValue = StringValue(FallBacks.id)

  object Keys {
    val readDriver: String = "readDriver"
    val restrictDriverToExome: String = "restrictDriverToExome"
    val restrictMissesToExome: String = "restrictMissesToExome"
    val canonicalizeDriver: String = "canonicalizeDriver"
    val readExtractedData: String = "readExtractedData"
    val canonicalizeExtractedData: String = "canonicalizeExtractedData"
    val cacheMisses: String = "cacheMisses"
    val writeCacheMisses: String = "writeCacheMisses"
    val readVepOutput: String = "readVepOutput"
    val join: String = "join"
    val calculateMaf: String = "calculateMaf"
    val filter: String = "filter"
    val write: String = "write"
  }

  def buildPhaseOneRequest(): Request = {
    val requestId = "lunaris_vep_phase_one_" + jobId.toString
    val toolCalls = {
      Map(
        Keys.cacheMisses -> ToolCalls.vcfRecordsReader(cacheMissesFile, chromsValue),
        Keys.restrictMissesToExome -> ToolCalls.restrictToRegions(Keys.cacheMisses, exomesFile),
        Keys.writeCacheMisses ->
          ToolCalls.vcfRecordsWriter(Keys.restrictMissesToExome, vepInputFile, refFieldVcf, altFieldVcf)
      )
    }
    Request(requestId, regions, Recipe(toolCalls))
  }

  def buildPhaseTwoRequest(): Request = {
    val requestId = "lunaris_vep_phase_two_" + jobId.toString
    val toolCalls =
      Map(
        Keys.readDriver -> ToolCalls.vcfRecordsReader(driverFile, chromsValue),
        Keys.restrictDriverToExome -> ToolCalls.restrictToRegions(Keys.readDriver, exomesFile),
        Keys.canonicalizeDriver ->
          ToolCalls.idCanonicalizer(Keys.restrictDriverToExome, refFieldVcf, altFieldVcf, idFieldNew),
        Keys.readExtractedData -> ToolCalls.vepRecordsReader(extractedDataFile, chromsValue),
        Keys.canonicalizeExtractedData ->
          ToolCalls.idCanonicalizer(Keys.readExtractedData, refFieldVep, altFieldVep, idFieldNew),
        Keys.readVepOutput -> ToolCalls.vepRecordsReader(vepOutputFile, chromsValue),
        Keys.join ->
          ToolCalls.joinRecordsWithFallback(
            Keys.canonicalizeDriver, Seq(Keys.canonicalizeExtractedData, Keys.readVepOutput), idFallback
          ),
        Keys.calculateMaf -> ToolCalls.calculateMaf(Keys.join),
        Keys.filter -> ToolCalls.filter(Keys.calculateMaf, filterValue),
        Keys.write -> ToolCalls.groupFileWriter(Keys.filter, outputFileValue, outputFileFormatValue)
      )
    Request(requestId, regions, Recipe(toolCalls))
  }
}

