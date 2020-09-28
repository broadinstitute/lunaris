package lunaris.app

import scala.language.reflectiveCalls

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

  trait RequestFile {
    _: ScallopConf =>
    val configFile = opt[String](descr = "Configuration file")
  }

  val batch = new Subcommand("batch") with RequestFile {
    banner("Loads a request from file or Google Cloud Storage object and executes it.")
    val requestFile = opt[String](descr = "Location of file containing request in JSON.")
  }
  addSubcommand(batch)

  trait WebService {
    _: ScallopConf =>
    val webInterface = opt[String](descr = "Web interface to bind to, e.g. localhost, 0.0.0.0")
    val port = opt[Int](descr = "Port to bind to, e.g. 80, 8080")
  }

  val server = new Subcommand("server") with RequestFile with WebService {
    banner("Web service: accepts HTML POST requests at http://<host>/lunaris/query \n" +
      "and offers a WebUI at http://<host>/lunaris.lunaris.html")
  }
  addSubcommand(server)
  val vep = new Subcommand("vep") with RequestFile with WebService {
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

  private def copyUniversalOptions(configKitBox: BuilderBox[LunarisConfigProps], configFileOwner: RequestFile):
  Unit = {
    configKitBox.modifyForeach(configFileOwner.configFile.map(InputId(_)).toOption)(_.configFile.set(_))
  }

  private def copyServerOptions(configKitBox: BuilderBox[LunarisConfigProps], webService: WebService): Unit = {
    configKitBox.modifyForeach(webService.webInterface.toOption)(_.webInterface.set(_))
    configKitBox.modifyForeach(webService.port.toOption)(_.port.set(_))
  }

  def toConfigProps: LunarisConfigProps = {
    val configPropsBox: BuilderBox[LunarisConfigProps] = BuilderBox(LunarisConfigProps.empty)
    subcommands match {
      case List(this.batch) =>
        configPropsBox.modify(_.mode.set(LunarisMode.Batch))
        copyUniversalOptions(configPropsBox, this.batch)
        configPropsBox.modifyForeach(this.batch.requestFile.map(InputId(_)).toOption)(_.requestFile.set(_))
      case List(this.server) =>
        configPropsBox.modify(_.mode.set(LunarisMode.Server))
        copyUniversalOptions(configPropsBox, this.server)
        copyServerOptions(configPropsBox, this.server)
      case List(this.vep) =>
        configPropsBox.modify(_.mode.set(LunarisMode.Vep))
        copyUniversalOptions(configPropsBox, this.vep)
        copyServerOptions(configPropsBox, this.vep)
        configPropsBox.modifyForeach(this.vep.inputsFolder.map(File(_)).toOption)(_.inputsFolder.set(_))
        configPropsBox.modifyForeach(this.vep.resultsFolder.map(File(_)).toOption)(_.resultsFolder.set(_))
        configPropsBox.modifyForeach(this.vep.dataFile.map(InputId(_)).toOption)(_.dataFile.set(_))
        configPropsBox.modifyForeach(this.vep.indexFile.map(InputId(_)).toOption)(_.indexFile.set(_))
    }
    configPropsBox.value
  }
}
