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
    val defaultConfigBox = LunarisConfigKit.default
    val configBox = cliConfigBox.withFallback(defaultConfigBox)
    cliConfigBox.mode.get match {
      case Right(mode) =>
        mode match {
          case LunarisMode.Batch =>
            val input = InputId(lunarisCliConf.batch.requestFile())
            BatchRunner.run(input)
          case LunarisMode.Server =>
            configBox.toServerSettings match {
              case Left(snag) =>
                println("Cannot start Lunaris web server")
                println(snag.message)
              case Right(serverSettings) =>
                ServerRunner.run(serverSettings)
            }
          case LunarisMode.Vep =>
            configBox.toVepServerSettings match {
              case Left(snag) =>
                println("Cannot start Lunaris VEP Mask Server")
                println(snag.message)
                println("Exit")
              case Right(vepServerSettings) =>
                VepServerRunner.run(vepServerSettings)
            }
        }
      case Left(snag) =>
        println("Invalid invocation")
        println(snag.message)
    }

  }
}
