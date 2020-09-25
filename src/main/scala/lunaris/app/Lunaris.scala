package lunaris.app

import better.files.File
import lunaris.data.BlockGzippedWithIndex

import scala.language.reflectiveCalls
import lunaris.io.InputId

object Lunaris {
  def main(args: Array[String]): Unit = {
    println(s"This is ${LunarisInfo.versionLong}")
    val lunarisCliConf = new LunarisCliConf(args)
    val cliConfigBox = lunarisCliConf.toConfigBox
    cliConfigBox.mode.get match {
      case Right(mode) =>
        mode match {
          case LunarisMode.Batch =>
            val input = InputId(lunarisCliConf.batch.requestFile())
            BatchRunner.run(input)
          case LunarisMode.Server =>
            ServerRunner.run(lunarisCliConf.server.host.toOption, lunarisCliConf.server.port.toOption)
          case LunarisMode.Vep =>
            val dataFileWithIndex = {
              BlockGzippedWithIndex(
                lunarisCliConf.vep.dataFile(),
                lunarisCliConf.vep.indexFile.toOption
              )
            }
            var cliConfigBoxTemp: LunarisConfigBox = LunarisConfigBox.empty
            lunarisCliConf.vep.host.toOption.foreach { host => cliConfigBoxTemp = cliConfigBoxTemp.host.set(host) }
            val defaultConfigBox = LunarisConfigBox.default
            val configBox = cliConfigBoxTemp.withFallback(defaultConfigBox)
            configBox.toVepServerSettings match {
              case Left(snag) =>
                println("Cannot start VEP Mask Server")
                println(snag.message)
                println("Exit")
              case Right(vepServerSettings) =>
                VepServerRunner.run(
                  vepServerSettings,
                  lunarisCliConf.vep.port.toOption,
                  lunarisCliConf.vep.inputsFolder.map(File(_))(),
                  lunarisCliConf.vep.resultsFolder.map(File(_))(),
                  dataFileWithIndex,
                  lunarisCliConf.vep.varId()
                )
            }
        }
      case Left(snag) =>
        println("Invalid invocation")
        println(snag.message)
    }

  }
}
