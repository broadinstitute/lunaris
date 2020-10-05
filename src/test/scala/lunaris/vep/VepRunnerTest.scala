package lunaris.vep

import lunaris.app.{LunarisConfigProps, VepSettings}
import lunaris.io.InputId
import org.scalatest.funsuite.AnyFunSuite

class VepRunnerTest extends AnyFunSuite {

  private val configFile = InputId("/home/oliverr/git/lunaris/configs/oliversBroadWindowsLaptop.conf")
  private val snagOrVepSettings = LunarisConfigProps.inputIdProps(configFile).flatMap(props => props.toVepSettings)

  private def runVep(vepSettings: VepSettings, chrom: String, pos: Int, ref: String, alt: String): Unit = {
    val vepRunner = new VepRunner(vepSettings.runSettings)
    val id = s"$chrom:$pos:$ref:$alt"
    val qual = ""
    val filter = ""
    val info = ""
    val format = ""
    val snagOrValues = vepRunner.calculateValues(id, chrom, pos, ref, alt, qual, filter, info, format)
    assert(snagOrValues.isRight)
    val Right((headers, values)) = snagOrValues
    assert(headers.length == values.length)
    assert(headers.length > 33)
  }

  test("Run vep for 1:69088:T:G") {
    assert(snagOrVepSettings.isRight)
    val vepSettings = snagOrVepSettings.toOption.get
    runVep(vepSettings, "1", 69088, "T", "G")
  }

  test("Run vep for 1:69088:T:GG") {
    assert(snagOrVepSettings.isRight)
    val vepSettings = snagOrVepSettings.toOption.get
    runVep(vepSettings, "1", 69088, "T", "GG")
  }
}
