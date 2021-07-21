package lunaris.selene

import better.files.File
import lunaris.vep.VepOutputReader
import org.scalatest.funsuite.AnyFunSuite

class SeleneTest extends AnyFunSuite {
  test("Run selene tabix") {
    val inputFile = File("/home/oliverr/lunaris/vep/test/inputs/input_selene.vcf")
    val dataFile = File("/home/oliverr/lunaris/vep/data/all_sites.vep.tsv.gz")
    val regionsFile = File("/home/oliverr/lunaris/vep/aux/55k.gencode.transcript.exons")
    val refCol = VepOutputReader.FileColNames2.ref
    val altCol = VepOutputReader.FileColNames2.alt
    val outFolder = File("tmp")
    val outFile = outFolder / "out.tsv"
    val cacheMissesFile = outFolder / "misses.vcf"
    val result = Selene.tabix(inputFile, dataFile, None, Some(regionsFile), refCol, altCol, outFile, cacheMissesFile)
    assert(result.isRight, result.left.toOption.map(_.message).getOrElse(""))
  }
}
