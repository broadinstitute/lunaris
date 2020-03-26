package lunaris.app

import lunaris.data.DataSources
import lunaris.io.ResourceConfig
import lunaris.io.bgz.BGZReader

object LunarisApp {

  def main(args: Array[String]): Unit = {
    //    val dataSourceWithIndex = DataSources.simDataOnTerra
    val dataSourceWithIndex = DataSources.simDataOnOliversOldLaptop
    dataSourceWithIndex.index.newReadChannelDisposable(ResourceConfig.empty).useUp { readChannel =>
      val reader = BGZReader(readChannel)
    }
  }

}
