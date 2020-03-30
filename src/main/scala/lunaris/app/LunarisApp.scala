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
    dataSourceWithIndex.index.newReadChannelDisposable(ResourceConfig.empty).useUp { readChannel =>
      val snagOrBlock = BGZBlock.read(readChannel)
      println("Snag or Block:")
      println(snagOrBlock)
      println("That was the Snag or Block")
    }
  }

}
