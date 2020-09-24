package lunaris.app

import better.files.File
import lunaris.data.BlockGzippedWithIndex

import scala.language.reflectiveCalls
import lunaris.io.InputId

object Lunaris {
  def main(args: Array[String]): Unit = {
    println(s"This is ${LunarisInfo.versionLong}")
    val conf = new LunarisCliConf(args)
    conf.subcommands match {
      case List(conf.batch) =>
        val input = InputId(conf.batch.requestFile())
        BatchRunner.run(input)
      case List(conf.server) =>
        ServerRunner.run(conf.server.host.toOption, conf.server.port.toOption)
      case List(conf.vep) =>
        val dataFileWithIndex = {
          BlockGzippedWithIndex(
            conf.vep.dataFile(),
            conf.vep.indexFile.toOption
          )
        }
        val vepSettings = new VepSettings()
        VepServerRunner.run(
          vepSettings,
          conf.vep.host.toOption,
          conf.vep.port.toOption,
          conf.vep.inputsFolder.map(File(_))(),
          conf.vep.resultsFolder.map(File(_))(),
          dataFileWithIndex,
          conf.vep.varId()
        )
    }
  }
}
