package lunaris.app

import scala.language.reflectiveCalls

import lunaris.io.InputId

object Lunaris {
  def main(args: Array[String]): Unit = {
    val conf = new LunarisConf(args)
    conf.subcommands match {
      case List(conf.batch) =>
        val input = InputId(conf.batch.requestFile())
        BatchRunner.run(input)
      case List(conf.server) =>
        ServerRunner.run()
    }
  }
}
