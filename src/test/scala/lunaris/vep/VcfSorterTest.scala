package lunaris.vep

import better.files.File
import org.scalatest.funsuite.AnyFunSuite
import scala.sys.process._

import java.nio.charset.StandardCharsets

class VcfSorterTest extends AnyFunSuite {
  private val t = "\t"
  private val vcfContentUnsorted =
    s"""#CHROM${t}POS${t}ID${t}REF${t}ALT${t}QUAL${t}FILTER${t}INFO${t}FORMAT
       |1${t}196857281${t}1:196857281_A/ATGT${t}A${t}ATGT
       |3${t}33166006${t}3:33166006_G/GTC${t}G${t}GTC
       |1${t}193038186${t}1:193038186_T/TGGA${t}T${t}TGGA
       |2${t}179412617${t}2:179412617_T/TG${t}T${t}TG
       |1${t}200860688${t}1:200860688_T/TA${t}T${t}TA
       |2${t}179330497${t}2:179330497_C/CTA${t}C${t}CTA
       |""".stripMargin

  private val vcfContentSorted =
    s"""#CHROM${t}POS${t}ID${t}REF${t}ALT${t}QUAL${t}FILTER${t}INFO${t}FORMAT
       |1${t}193038186${t}1:193038186_T/TGGA${t}T${t}TGGA
       |1${t}196857281${t}1:196857281_A/ATGT${t}A${t}ATGT
       |1${t}200860688${t}1:200860688_T/TA${t}T${t}TA
       |2${t}179330497${t}2:179330497_C/CTA${t}C${t}CTA
       |2${t}179412617${t}2:179412617_T/TG${t}T${t}TG
       |3${t}33166006${t}3:33166006_G/GTC${t}G${t}GTC
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
