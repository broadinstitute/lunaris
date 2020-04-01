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
      val snagOrBlocks = BGZBlock.readThreeBlocks(readChannel)
      snagOrBlocks match {
        case Left(snag) =>
          println(snag.message)
          println(snag.report)
        case Right((block1, block2, block3)) =>
          println(block1)
          println(block2)
          println(block3)
          println(s"Size of unzipped data, block 1: " + block1.unzippedData.bytes.length)
          println(s"Size of unzipped data, block 2: " + block2.unzippedData.bytes.length)
          println(s"Size of unzipped data, block 3: " + block3.unzippedData.bytes.length)
          val string1 = new String(block1.unzippedData.bytes)
          val string2 = new String(block2.unzippedData.bytes)
          val string3 = new String(block3.unzippedData.bytes)
          println(string1 + "\nyo!\n"+ string2+ "\nyo!\n"+ string3)
      }
    }
  }

}
