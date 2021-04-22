package lunaris.vep

import akka.actor.ActorSystem
import akka.stream.Materializer
import lunaris.app.{LunarisConfigProps, VepSettings}
import lunaris.io.InputId
import lunaris.vep.vcf.VcfCore
import org.scalatest.funsuite.AnyFunSuite

import java.net.InetAddress

class VepRunnerTest extends AnyFunSuite {

  private val actorSystem = ActorSystem("test")
  private implicit val materializer: Materializer = Materializer(actorSystem)

  private val configFile = InputId("configs/oliversBroadWindowsLaptop.conf")
  private val snagOrVepSettings = LunarisConfigProps.inputIdProps(configFile).flatMap(props => props.toVepSettings)

  private def runVep(vepSettings: VepSettings, chrom: String, pos: Int, ref: String, alt: String): Unit = {
    val vepFolders = VepFolders(vepSettings)
    val scriptRepo = ScriptRepo(vepFolders.workFolder)
    val vepRunner = new VepRunner(vepFolders, scriptRepo, vepSettings.runSettings)
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

  private val devHostName = "GP7E3-45D"

  private def cancelIfOnDevWhereVepDoesNotRun(): Unit = {
    if(InetAddress.getLocalHost.getHostName == devHostName) {
      cancel(s"Skipping test since VEP does not run on $devHostName.")
    }
  }

  test("Run vep for 1:69088:T:G") {
    cancelIfOnDevWhereVepDoesNotRun()
    assert(snagOrVepSettings.isRight, snagOrVepSettings.left.toOption.getOrElse(""))
    val vepSettings = snagOrVepSettings.toOption.get
    runVep(vepSettings, "1", 69088, "T", "G")
  }

  test("Run vep for 1:69088:T:GG") {
    cancelIfOnDevWhereVepDoesNotRun()
    assert(snagOrVepSettings.isRight, snagOrVepSettings.left.toOption.getOrElse(""))
    val vepSettings = snagOrVepSettings.toOption.get
    runVep(vepSettings, "1", 69088, "T", "GG")
  }
}
