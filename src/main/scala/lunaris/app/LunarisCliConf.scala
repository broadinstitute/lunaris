package lunaris.app

import better.files.File
import lunaris.io.InputId
import lunaris.utils.BuilderBox
import org.rogach.scallop.{ScallopConf, Subcommand}

class LunarisCliConf(arguments: Seq[String]) extends ScallopConf(arguments) {
  version(LunarisInfo.versionLong)
  banner(
    """Usage: lunaris batch|server ...
      |Lunaris is a stream processor to extract, combine and munge genomics-related data from
      |location-sorted block-gzipped tabix-indexed files.
      |Files can be local, or on Google Cloud Storage, including on Terra.
      |""".stripMargin)
  footer("For more or more updated information, check https://github.com/broadinstitute/lunaris")
  val batch = new Subcommand("batch") {
    banner("Loads a request from file or Google Cloud Storage object and executes it.")
    val requestFile = opt[String](descr = "Location of file containing request in JSON.", required = true)
  }
  addSubcommand(batch)

  trait WebService {
    _: ScallopConf =>
    val host = opt[String](descr = "Host to bind to, e.g. localhost, 0.0.0.0", required = false)
    val port = opt[Int](descr = "Port to bind to, e.g. 80, 8080", required = false)
  }

  val server = new Subcommand("server") with WebService {
    banner("Web service: accepts HTML POST requests at http://<host>/lunaris/query \n" +
      "and offers a WebUI at http://<host>/lunaris.lunaris.html")
  }
  addSubcommand(server)
  val vep = new Subcommand("vep") with WebService {
    banner("Ensembl VEP.")
    val inputsFolder = opt[String](descr = "Folder to store inputs.")
    val resultsFolder = opt[String](descr = "Folder to store results.")
    val dataFile = opt[String](descr = "File with variant data")
    val indexFile =
      opt[String](descr = "Index file for variant data (if absent, it will be data file plus .tbi")
    val varId = opt[String](descr = "Name of column with variant id")
  }
  addSubcommand(vep)
  requireSubcommand()
  verify()

  private def withServerOptions(configKitBox: BuilderBox[LunarisConfigKit], webService: WebService): Unit = {
    configKitBox.modifyForeach(webService.host.toOption)(_.host.set(_))
    configKitBox.modifyForeach(webService.port.toOption)(_.port.set(_))
  }

  def toConfigBox: LunarisConfigKit = {
    var configKitBox: BuilderBox[LunarisConfigKit] = BuilderBox(LunarisConfigKit.empty)
    subcommands match {
      case List(this.batch) =>
        configKitBox.modify(_.mode.set(LunarisMode.Batch))
      case List(this.server) =>
        configKitBox.modify(_.mode.set(LunarisMode.Server))
        withServerOptions(configKitBox, this.server)
      case List(this.vep) =>
        configKitBox.modify(_.mode.set(LunarisMode.Vep))
        withServerOptions(configKitBox, this.vep)
        configKitBox.modifyForeach(this.vep.inputsFolder.map(File(_)).toOption)(_.inputsFolder.set(_))
        configKitBox.modifyForeach(this.vep.resultsFolder.map(File(_)).toOption)(_.resultsFolder.set(_))
        configKitBox.modifyForeach(this.vep.dataFile.map(InputId(_)).toOption)(_.dataFile.set(_))
        configKitBox.modifyForeach(this.vep.indexFile.map(InputId(_)).toOption)(_.indexFile.set(_))
    }
    configKitBox.value
  }
}
