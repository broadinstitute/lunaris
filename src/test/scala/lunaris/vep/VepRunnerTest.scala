package lunaris.vep

import akka.actor.ActorSystem
import akka.stream.Materializer
import lunaris.app.{LunarisConfigProps, VepSettings}
import lunaris.io.InputId
import lunaris.vep.vcf.VcfCore
import org.scalatest.funsuite.AnyFunSuite

class VepRunnerTest extends AnyFunSuite {

  private val actorSystem = ActorSystem("test")
  private implicit val materializer: Materializer = Materializer(actorSystem)

  private val configFile = InputId("configs/oliversBroadWindowsLaptop.conf")
  private val snagOrVepSettings = LunarisConfigProps.inputIdProps(configFile).flatMap(props => props.toVepSettings)

  private def runVep(vepSettings: VepSettings, chrom: String, pos: Int, ref: String, alt: String): Unit = {
    val vepRunner = new VepRunner(vepSettings.runSettings)
    val id = s"$chrom:$pos:$ref:$alt"
    val qual = ""
    val filter = ""
    val info = ""
    val format = ""
    val vcfRecord = VcfCore.VcfCoreRecord(chrom, pos, id, ref, alt, qual, filter, info, format)
    val snagOrValues = vepRunner.calculateValues(vcfRecord, snag => println(snag.message))
    assert(snagOrValues.isRight, snagOrValues.left.toOption.map(_.message).getOrElse(""))
    val Right(record) = snagOrValues
    assert(record.values.size > 33)
  }

  test("Run vep for 1:69088:T:G") {
    assert(snagOrVepSettings.isRight, snagOrVepSettings.left.toOption.getOrElse(""))
    val vepSettings = snagOrVepSettings.toOption.get
    runVep(vepSettings, "1", 69088, "T", "G")
  }

  test("Run vep for 1:69088:T:GG") {
    assert(snagOrVepSettings.isRight, snagOrVepSettings.left.toOption.getOrElse(""))
    val vepSettings = snagOrVepSettings.toOption.get
    runVep(vepSettings, "1", 69088, "T", "GG")
  }
}
