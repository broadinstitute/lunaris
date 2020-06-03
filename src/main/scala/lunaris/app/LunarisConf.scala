package lunaris.app

import org.rogach.scallop.{ScallopConf, Subcommand}

class LunarisConf(arguments: Seq[String]) extends ScallopConf(arguments) {
  val batch = new Subcommand("batch") {
    val requestFile = opt[String](required = true)
  }
  addSubcommand(batch)
  val server = new Subcommand("server") {

  }
  addSubcommand(server)
  verify()
}
