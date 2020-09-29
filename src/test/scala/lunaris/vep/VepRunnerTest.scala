package lunaris.vep

import better.files.File
import lunaris.app.LunarisConfigProps
import lunaris.io.InputId
import org.scalatest.funsuite.AnyFunSuite

class VepRunnerTest extends AnyFunSuite {

  test("Writing input file") {
    val configFile = InputId("/home/oliverr/git/lunaris/configs/oliversBroadWindowsLaptop.conf")
    val snagOrVepSettings = LunarisConfigProps.inputIdProps(configFile).flatMap(props => props.toVepSettings)
    assert(snagOrVepSettings.isRight)
    val vepSettings = snagOrVepSettings.toOption.get
    val vepRunner = new VepRunner(vepSettings.runSettings)
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
    assert(snagOrValues.isRight)
    val Right((headers, values)) = snagOrValues
    assert(headers.length == values.length)
    assert(headers.length > 33)
  }

}
