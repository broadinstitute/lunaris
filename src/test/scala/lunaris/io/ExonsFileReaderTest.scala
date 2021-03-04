package lunaris.io

import better.files.File
import org.scalatest.funsuite.AnyFunSuite

import java.nio.charset.StandardCharsets

final class ExonsFileReaderTest extends AnyFunSuite {

  private val content =
    """ENST00000000233\t7\t127228399\t127228552
      |ENST00000000233\t7\t127228552\t127228619
      |ENST00000000233\t7\t127229137\t127229217
      |ENST00000000233\t7\t127229539\t127229648
      |ENST00000000233\t7\t127230120\t127230191
      |ENST00000000233\t7\t127231017\t127231142
      |ENST00000000233\t7\t127231267\t127231351
      |ENST00000000233\t7\t127231351\t127231759
      |ENST00000000412\t12\t9092961\t9094416
      |ENST00000000412\t12\t9094416\t9094536
      |ENST00000000412\t12\t9095012\t9095138
      |ENST00000000412\t12\t9096001\t9096131
      |ENST00000000412\t12\t9096397\t9096506
      |ENST00000000412\t12\t9098014\t9098180
      |ENST00000000412\t12\t9098825\t9099001
      |ENST00000000412\t12\t9102084\t9102551
      |ENST00000000442\t11\t64073050\t64073208
      |ENST00000000442\t11\t64074640\t64074651
      |ENST00000000442\t11\t64074651\t64074976
      |ENST00000000442\t11\t64081423\t64081539
      |ENST00000000442\t11\t64081711\t64081839
      |ENST00000000442\t11\t64082213\t64082383
      |ENST00000000442\t11\t64082473\t64082742
      |ENST00000000442\t11\t64083179\t64083436
      |ENST00000000442\t11\t64083436\t64084210
      |""".stripMargin.replace("\\t", "\t")

  test("Read file") {
    val inFile = File.newTemporaryFile("ExonsFileReaderTest", "exons")
    inFile.append(content)(StandardCharsets.UTF_8)
    val in = FileInputId(inFile)
    val snagOrLociSet = ExonsFileReader.read(in)
    assert(snagOrLociSet.isRight, snagOrLociSet)
    val lociSet = snagOrLociSet.toOption.get
    assert(lociSet.size == 20)
  }

}
