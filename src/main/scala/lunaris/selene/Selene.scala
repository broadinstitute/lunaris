package lunaris.selene

import better.files.File
import org.broadinstitute.yootilz.core.snag.Snag

import java.nio.charset.StandardCharsets
import scala.sys.process.stringToProcess

object Selene {

  def runCommandLine(commandLine: String): Either[Snag, Unit] = {
    val returnValue = commandLine.!
    if (returnValue == 0) {
      Right(())
    } else {
      Left(Snag(s"Return value is not 0, but $returnValue."))
    }
  }

  def tabix(inputFile: File, dataFile: File, indexFileOpt: Option[File], regionsFileOpt: Option[File],
            refCol: String, altCol: String, outFile: File, cacheMissesFile: File):
  Either[Snag, Unit] = {
    val indexFilePart = indexFileOpt.map(indexFile => s"--index-file $indexFile").getOrElse("")
    val regionsFilePart = regionsFileOpt.map(regionsFile => s"--regions-file $regionsFile").getOrElse("")
    val commandLine =
      s"selene tabix --input-file $inputFile --data-file $dataFile $indexFilePart $regionsFilePart " +
        s"--col-ref $refCol --col-alt $altCol " +
        s"--output-file $outFile --cache-misses-file $cacheMissesFile"
    println(commandLine)
    runCommandLine(commandLine)
  }

  val chromosomeLinePrefix: String = "##contig=<"
  val idEntryPrefix: String = "ID="

  def readChromosomeList(file: File): Either[Snag, Seq[String]] = {
    val chroms =
      file.lineIterator(StandardCharsets.UTF_8).takeWhile(_.startsWith("##"))
        .filter(_.startsWith(chromosomeLinePrefix))
        .flatMap { line =>
          val lineTrimmed = line.trim()
          val entries =
            lineTrimmed.substring(chromosomeLinePrefix.length, lineTrimmed.length - 1).split(",").toSeq
          entries.find(_.startsWith(idEntryPrefix)).map(_.substring(idEntryPrefix.length))
        }.toSeq
    if (chroms.isEmpty) {
      Left(Snag("Missing chromosome meta-lines."))
    } else {
      Right(chroms)
    }
  }

  def run_script(script: Mion.Script): Either[Snag, Unit] = {
    val scriptFile = File.newTemporaryFile("lunaris_selene", ".sh")
    scriptFile.overwrite(script.to_code())
    val commandLine = s"selene script $scriptFile"
    println(commandLine)
    runCommandLine(commandLine)
  }
}
