package lunaris.app

import java.io.InputStream
import java.nio.file.Files

import lunaris.data.DataSources

object LunarisApp {

  def main(args: Array[String]): Unit = {
    //    val dataSourceWithIndex = DataSources.simDataOnTerra
    val dataSourceWithIndex = DataSources.simDataOnOliversOldLaptop
    val featureResource = dataSourceWithIndex.dataSource.toUri.toString
    println(featureResource)
    val rawIndexInputStream: InputStream = Files.newInputStream(dataSourceWithIndex.index)
    val chromCol = 0
    val posCol = 1
  }

}
