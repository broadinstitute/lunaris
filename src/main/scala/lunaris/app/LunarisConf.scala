package lunaris.app

import org.rogach.scallop.{ScallopConf, Subcommand}

class LunarisConf(arguments: Seq[String]) extends ScallopConf(arguments) {
  version(LunarisInfo.versionLong)
  banner(
    """Usage: lunaris batch|server ...
      |Lunaris is a stream processor to extract, combine and munge genomics-related data from location-sorted
      |block-gzipped tabix-indexed files.
      |Files can be local, or on Google Cloud Storage, including on Terra.
      |""".stripMargin)
  footer("For more or more updated information, check https://github.com/broadinstitute/lunaris")
  val batch = new Subcommand("batch") {
    banner("Loads a request from file and executes it.")
    val requestFile = opt[String](descr = "Location of file containing request in JSON.", required = true)
  }
  addSubcommand(batch)
  val server = new Subcommand("server") {
    banner("Web service - not yet implemented.")
  }
  addSubcommand(server)
  requireSubcommand()
  verify()
}
