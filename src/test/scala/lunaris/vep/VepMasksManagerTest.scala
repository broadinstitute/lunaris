package lunaris.vep

import better.files.{File, Resource}
import lunaris.recipes.parsing.RecordExpressionParser
import org.scalatest.funsuite.AnyFunSuite

class VepMasksManagerTest extends AnyFunSuite {
  test("List of masks") {
    val masksFolder: File = File("src/main/resources/web/masks")
    assert(masksFolder.exists)
    val maskFiles = masksFolder.children.filter(_.`extension`.contains(".lun")).map(_.name).toSet
    val maskFilesFromMasksList = VepMasksManager.maskNames.map(_ + ".lun").toSet
    assert(maskFiles == maskFilesFromMasksList)
  }

  test("Masks are not just whitespace") {
    for (maskName <- VepMasksManager.maskNames) {
      val maskPath = VepMasksManager.getPathForMask(maskName)
      val maskStringOpt = Resource.asString(maskPath)
      assert(maskStringOpt.nonEmpty, s"Could not load mask $maskName.")
      val maskString = maskStringOpt.get
      assert(maskString.trim.nonEmpty, s"$maskName consists of only whitespace")
    }
  }

  test("Parsing masks") {
      for (maskName <- VepMasksManager.maskNames) {
      val maskPath = VepMasksManager.getPathForMask(maskName)
      val maskStringOpt = Resource.asString(maskPath)
      assert(maskStringOpt.nonEmpty, s"Could not load mask $maskName.")
      val maskString = maskStringOpt.get
      val snagOrRecordExpression = RecordExpressionParser.parse(maskString)
      val shouldNeverBeNeeded = ""
      val parseErrorMessage = snagOrRecordExpression.left.toOption.map(_.report).getOrElse(shouldNeverBeNeeded)
      val errorMessage = s"$maskName does not parse: " + parseErrorMessage
      assert(snagOrRecordExpression.isRight, errorMessage)
    }
  }
}
