package lunaris.app

import lunaris.data.DataSources
import lunaris.io.ResourceConfig
import lunaris.io.bgz.BGZBlock

object LunarisApp {

  def main(args: Array[String]): Unit = {
    val usingLocalSimulatedDataOnOliversLaptop: Boolean = true
    val dataSourceWithIndex = if (usingLocalSimulatedDataOnOliversLaptop)
      DataSources.simDataOnOliversOldLaptop
    else
      DataSources.simDataOnTerra
    dataSourceWithIndex.dataSource.newReadChannelDisposable(ResourceConfig.empty).useUp { readChannel =>
      val snagOrBlock = BGZBlock.read(readChannel)
      snagOrBlock match {
        case Left(snag) =>
          println(snag.message)
          println(snag.report)
        case Right(block) =>
          println(block)
          println(s"Size of unzipped data: " + block.unzippedData.bytes.length)
          println(new String(block.unzippedData.bytes))
      }
    }
  }

}
