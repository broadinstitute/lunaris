package lunaris.vep

import better.files.{File, Resource}
import lunaris.recipes.parsing.RecordExpressionParser
import org.scalatest.funsuite.AnyFunSuite

class VepMasksManagerTest extends AnyFunSuite {
  test("List of masks") {
    val masksFolder: File = File("src/main/resources/web/masks")
    assert(masksFolder.exists)
    println(masksFolder.children.map(_.`extension`).mkString(", "))
    val maskFiles = masksFolder.children.filter(_.`extension`.contains(".lun")).map(_.name).toSeq
    val maskFilesFromMasksList = VepMasksManager.maskNames.map(_ + ".lun")
    assert(maskFiles == maskFilesFromMasksList)
  }

  test("Parsing masks") {
    for(maskName <- VepMasksManager.maskNames) {
      val maskPath = VepMasksManager.getPathForMask(maskName)
      val maskStringOpt =Resource.asString(maskPath)
      assert(maskStringOpt.nonEmpty, s"Could not load mask $maskName.")
      val maskString = maskStringOpt.get
      val snagOrRecordExpression = RecordExpressionParser.parse(maskString)
      println(snagOrRecordExpression)
      val shouldNeverBeNeeded = ""
      assert(snagOrRecordExpression.isRight, snagOrRecordExpression.left.toOption.getOrElse(shouldNeverBeNeeded))
    }
  }
}
