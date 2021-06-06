package lunaris.selene

import better.files.File
import org.broadinstitute.yootilz.core.snag.Snag

import scala.sys.process.stringToProcess

object Selene {

  def tabix(inputFile: File, dataFile: File, indexFileOpt: Option[File], refCol: String, altCol: String):
  Either[Snag, Unit] = {
    val indexFilePart = indexFileOpt.map(indexFile => s"--index-file $indexFile").getOrElse("")
    val commandLine =
      s"selene tabix --input-file $inputFile --data-file $dataFile $indexFilePart " +
      s"--col-ref $refCol --col-alt $altCol"
    println(commandLine)
    val returnValue = commandLine.!
    if(returnValue == 0) {
      Right(())
    } else {
      Left(Snag(s"Return value is not 0, but $returnValue."))
    }
  }

}
