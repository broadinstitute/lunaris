package lunaris.app

import scala.language.reflectiveCalls

object Lunaris {
  def main(args: Array[String]): Unit = {
    println(s"This is ${LunarisInfo.versionLong}")
    LunarisConfigProps.allProps(args) match {
      case Left(snag) =>
        println("Problem obtaining configuration")
        println(snag.message)
      case Right(configProps) =>
        configProps.mode.get match {
          case Right(mode) =>
            mode match {
              case LunarisMode.Batch =>
                configProps.requestFile.get match {
                  case Left(snag) =>
                    println("Cannot run Lunaris in batch mode")
                    println(snag.message)
                  case Right(requestFile) =>
                    BatchRunner.run(requestFile)
                }
              case LunarisMode.Server =>
                configProps.toServerSettings match {
                  case Left(snag) =>
                    println("Cannot start Lunaris web server")
                    println(snag.message)
                  case Right(serverSettings) =>
                    ServerRunner.run(serverSettings)
                }
              case LunarisMode.Vep =>
                configProps.toVepServerSettings match {
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
}
