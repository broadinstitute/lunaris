package lunaris.app

import java.io.InputStream
import java.nio.channels.Channels
import java.nio.file.Files

import lunaris.data.DataSources

object LunarisApp {

  def main(args: Array[String]): Unit = {
    //    val dataSourceWithIndex = DataSources.simDataOnTerra
    val dataSourceWithIndex = DataSources.simDataOnOliversOldLaptop
    val indexPath = dataSourceWithIndex.index
    val featureResource = dataSourceWithIndex.dataSource.asString
    println(featureResource)
    val chromCol = 0
    val posCol = 1
  }

}
