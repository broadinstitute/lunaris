package lunaris.vep

import better.files.File
import lunaris.vep.VepJobManager.JobId

class VepJobFiles(vepFolders: VepFolders, jobId: JobId) {
  jobFolder.createDirectories()

  def jobFolder: File = vepFolders.runFolder / jobId.string

  def inputFileName: String = "input_" + jobId.string + ".vcf"

  def inputFile: File = vepFolders.inputsFolder / inputFileName

  def extractedDataFileName: String = "data_" + jobId.string + ".tsv"

  def extractedDataFile: File = jobFolder / extractedDataFileName

  def cacheMissesFileName: String = "cache_misses_" + jobId.string + ".vcf"

  def cacheMissesFile: File = jobFolder / cacheMissesFileName

  def vepOutputFileName: String = "vep_output_" + jobId.string + ".tsv"

  def vepOutputFile: File = jobFolder / vepOutputFileName

  def mergedFileName: String = "merged_" + jobId.string + ".tsv"

  def mergedFile: File = jobFolder / mergedFileName

  def outputFileName: String = jobId.string + ".tsv"

  def outputFile: File = vepFolders.resultsFolder / outputFileName

  def logFileName: String = jobId.string + ".log"

  def logFile: File = vepFolders.resultsFolder / logFileName


}

object VepJobFiles {
  def apply(vepFolders: VepFolders, jobId: JobId): VepJobFiles = new VepJobFiles(vepFolders, jobId)
}
