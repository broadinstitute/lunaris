package lunaris.vep

import better.files.File
import org.broadinstitute.yootilz.core.snag.Snag

import scala.sys.process._

class VcfSorter(scriptRepo: ScriptRepo) {
  def sortVcf(input: File, output: File): Either[Snag, Unit] = {
    val commandLine = s"bash ${scriptRepo.Files.sortVcf} $input $output"
    val returnValue = commandLine.!
    if(returnValue != 0) {
      Left(Snag(s"Return value of sort_vcf should be 0, but is $returnValue"))
    } else {
      Right(())
    }
  }
}

object VcfSorter {
  def apply(scriptRepo: ScriptRepo): VcfSorter = new VcfSorter(scriptRepo)
}
