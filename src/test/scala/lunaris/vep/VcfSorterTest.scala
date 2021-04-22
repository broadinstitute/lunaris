package lunaris.vep

import better.files.File
import org.scalatest.funsuite.AnyFunSuite
import scala.sys.process._

import java.nio.charset.StandardCharsets

class VcfSorterTest extends AnyFunSuite {
  private val vcfContentUnsorted =
    """#CHROM\tPOS\tID\tREF\tALT\tQUAL\tFILTER\tINFO\tFORMAT
      |1\t196857281\t1:196857281_A/ATGT\tA\tATGT
      |3\t33166006\t3:33166006_G/GTC\tG\tGTC
      |1\t193038186\t1:193038186_T/TGGA\tT\tTGGA
      |2\t179412617\t2:179412617_T/TG\tT\tTG
      |1\t200860688\t1:200860688_T/TA\tT\tTA
      |2\t179330497\t2:179330497_C/CTA\tC\tCTA
      |""".stripMargin

  private val vcfContentSorted =
    """#CHROM\tPOS\tID\tREF\tALT\tQUAL\tFILTER\tINFO\tFORMAT
      |1\t193038186\t1:193038186_T/TGGA\tT\tTGGA
      |1\t196857281\t1:196857281_A/ATGT\tA\tATGT
      |1\t200860688\t1:200860688_T/TA\tT\tTA
      |2\t179330497\t2:179330497_C/CTA\tC\tCTA
      |2\t179412617\t2:179412617_T/TG\tT\tTG
      |3\t33166006\t3:33166006_G/GTC\tG\tGTC
      |""".stripMargin

  def doTest(inputFileName: String, outputFileName: String): Unit = {
    val tempFolder = File.newTemporaryDirectory()
    try {
      val inputFile = tempFolder / inputFileName
      val outputFile = tempFolder / outputFileName
      inputFile.writeText(vcfContentUnsorted)(File.OpenOptions.default, StandardCharsets.UTF_8)
      val scriptRepo = ScriptRepo(tempFolder)
      val vcfSorter = VcfSorter(scriptRepo)
      val snagOr = vcfSorter.sortVcf(inputFile, outputFile)
      s"ls -ralt $tempFolder".!
      assert(snagOr.isRight, snagOr.left.toOption.map(_.message).getOrElse(""))
      val outFileContent = outputFile.contentAsString(StandardCharsets.UTF_8)
      assert(outFileContent == vcfContentSorted)
    } finally {
      tempFolder.delete()
    }

  }

  test("Sort VCF file to different location") {
    doTest("input.vcf", "output.vcf")
  }

  test("Sort VCF file overwriting original location") {
    doTest("file.vcf", "file.vcf")
  }
}
