package lunaris.selene

import better.files.File
import org.broadinstitute.yootilz.core.snag.Snag

import java.nio.charset.StandardCharsets
import scala.sys.process.stringToProcess

object Selene {

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
    val returnValue = commandLine.!
    if (returnValue == 0) {
      Right(())
    } else {
      Left(Snag(s"Return value is not 0, but $returnValue."))
    }
  }

  val chromosomeLinePrefix: String = "## Chromosomes:"

  def readChromosomeList(file: File): Either[Snag, Seq[String]] = {
    val chromsLineOpt =
      file.lineIterator(StandardCharsets.UTF_8).takeWhile(_.startsWith("##"))
        .find(_.startsWith(chromosomeLinePrefix))
    chromsLineOpt match {
      case None =>
        Left(Snag("Missing chromosome meta-line."))
      case Some(chromsLine) =>
        Right(chromsLine.substring(chromosomeLinePrefix.length).trim().split("\t").toSeq)
    }
  }
}
