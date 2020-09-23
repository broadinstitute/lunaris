package lunaris.app

import org.rogach.scallop.{ScallopConf, Subcommand}

class LunarisConf(arguments: Seq[String]) extends ScallopConf(arguments) {
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
  trait WebService { _: ScallopConf =>
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
    val inputsFolder = opt[String](descr = "Folder to store inputs.", required = true)
    val resultsFolder = opt[String](descr = "Folder to store results.", required = true)
    val dataFile = opt[String](descr = "File with variant data", required = true)
    val indexFile =
      opt[String](descr = "Index file for variant data (if absent, it will be data file plus .tbi", required = false)
    val varId = opt[String](descr = "Name of column with variant id", required = true)
  }
  addSubcommand(vep)
  requireSubcommand()
  verify()
}
