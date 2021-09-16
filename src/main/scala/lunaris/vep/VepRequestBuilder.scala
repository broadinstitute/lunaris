package lunaris.vep

import lunaris.expressions.LunBoolExpression
import lunaris.genomics.Region
import lunaris.io.request.Request
import lunaris.io.request.examples.RequestBuildUtils.ToolCalls
import lunaris.recipes.Recipe
import lunaris.recipes.values.LunType.StringType
import lunaris.recipes.values.LunValue.PrimitiveValue.{FileValue, StringValue}
import lunaris.recipes.values.LunValue.{ArrayValue, ExpressionValue}
import lunaris.vep.VepJobManager.JobId

case class VepRequestBuilder(jobId: JobId,
                             jobFiles: VepJobFiles,
                             chroms: Seq[String],
                             regions: Map[String, Seq[Region]],
                             filter: LunBoolExpression,
                             outFileFormat: String) {
  val chromsValue: ArrayValue = ArrayValue(chroms.map(StringValue), StringType)
  val mergedFile: FileValue = FileValue(jobFiles.mergedFile)
  val filterValue: ExpressionValue = ExpressionValue(filter)
  val outputFileValue: FileValue = FileValue(jobFiles.outputFile)
  val outputFileFormatValue: StringValue = StringValue(outFileFormat)

  object Keys {
    val readMerged: String = "readMerged"
    val join: String = "join"
    val calculateMaf: String = "calculateMaf"
    val filter: String = "filter"
    val write: String = "write"
  }

  def buildRequest(): Request = {
    val requestId = "lunaris_vep_" + jobId.toString
    val toolCalls =
      Map(
        Keys.readMerged -> ToolCalls.vepRecordsReader(mergedFile, chromsValue),
        Keys.calculateMaf -> ToolCalls.calculateMaf(Keys.readMerged),
        Keys.filter -> ToolCalls.filter(Keys.calculateMaf, filterValue),
        Keys.write -> ToolCalls.groupFileWriter(Keys.filter, outputFileValue, outputFileFormatValue)
      )
    Request(requestId, regions, Recipe(toolCalls))
  }
}

