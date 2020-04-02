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
      val snagOrBlocks = BGZBlock.readAllBlocks(readChannel)
      snagOrBlocks match {
        case Left(snag) =>
          println(snag.message)
          println(snag.report)
        case Right(blocks) =>
          blocks.foreach { block =>
            println(block.toString + ", size of unzipped data: " + block.unzippedData.bytes.length)
          }
//          println(blocks.map(_.unzippedData.bytes).map(new String(_)).mkString("\nyo\n"))
      }
    }
  }

}
