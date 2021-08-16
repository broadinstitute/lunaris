package lunaris.vep

import better.files.File
import lunaris.vep.VepJobManager.JobId

class VepJobFiles(vepFolders: VepFolders, jobId: JobId) {
  def jobFolder: File = vepFolders.runFolder / jobId.string;

  def inputFileName: String = "input_" + jobId.string + ".vcf"

  def inputFile: File = vepFolders.inputsFolder / inputFileName

  def extractedDataFileName: String = "data_" + jobId.string + ".tsv"

  def extractedDataFile: File = jobFolder / extractedDataFileName

  def cacheMissesFileName: String = "cache_misses_" + jobId.string + ".vcf"

  def cacheMissesFile: File = jobFolder / cacheMissesFileName

  def vepInputFileName: String = "vep_input_" + jobId.string + ".vcf"

  def vepInputFile: File = vepFolders.runFolder / vepInputFileName

  def vepOutputFileName: String = "vep_output_" + jobId.string + ".tsv"

  def vepOutputFile: File = vepFolders.runFolder / vepOutputFileName

  def outputFileName: String = jobId.string + ".tsv"

  def outputFile: File = vepFolders.resultsFolder / outputFileName

  def logFileName: String = jobId.string + ".log"

  def logFile: File = vepFolders.resultsFolder / logFileName


}

object VepJobFiles {
  def apply(vepFolders: VepFolders, jobId: JobId): VepJobFiles = new VepJobFiles(vepFolders, jobId)
}
