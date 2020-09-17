package lunaris.vep

import better.files.File
import org.scalatest.funsuite.AnyFunSuite

class VepRunnerTest extends AnyFunSuite {

  test("Writing input file") {
    val vepRunPath = File("/home/BROAD.MIT.EDU/oliverr/git/ensembl-vep/vep")
    val vepWorkDir = File("/home/BROAD.MIT.EDU/oliverr/lunaris/vep/run")
    val vepRunner = new VepRunner(VepInstallation.autoPick)
    val id = "1:69088:T:G"
    val chrom = "1"
    val pos = 69088
    val ref = "T"
    val alt = "G"
    val qual = ""
    val filter = ""
    val info = ""
    val format = ""
    val snagOrValues = vepRunner.calculateValues(id, chrom, pos, ref, alt, qual, filter, info, format)
    println(snagOrValues)
  }

}
