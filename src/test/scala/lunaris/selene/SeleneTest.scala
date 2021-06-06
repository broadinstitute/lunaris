package lunaris.selene

import better.files.File
import lunaris.vep.VepOutputReader
import org.scalatest.funsuite.AnyFunSuite

class SeleneTest extends AnyFunSuite {
  test("Run selene tabix") {
    val inputFile = File("/home/oliverr/lunaris/vep/test/inputs/input_selene.vcf")
    val dataFile = File("/home/oliverr/lunaris/vep/data/all_sites.vep.tsv.gz")
    val refCol = VepOutputReader.FileColNames2.ref
    val altCol = VepOutputReader.FileColNames2.alt
    val result = Selene.tabix(inputFile, dataFile, None, refCol, altCol)
    assert(result.isRight, result.left.toOption.map(_.message).getOrElse(""))
  }
}
