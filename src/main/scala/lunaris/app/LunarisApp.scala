package lunaris.app

import lunaris.data.DataSources
import lunaris.io.tbi.TBIFileHeader
import lunaris.io.{ByteBufferReader, ByteBufferRefiller, ResourceConfig}

object LunarisApp {
  def main(args: Array[String]): Unit = {
    val usingLocalSimulatedDataOnOliversLaptop: Boolean = true
    val dataSourceWithIndex = if (usingLocalSimulatedDataOnOliversLaptop)
      DataSources.simDataOnOliversOldLaptop
    else
      DataSources.simDataOnTerra
    dataSourceWithIndex.index.newReadChannelDisposable(ResourceConfig.empty).useUp { readChannel =>
      val bufferSize = 10000
      val refiller = ByteBufferRefiller.bgunzip(readChannel, bufferSize)
      val reader = new ByteBufferReader(refiller)
      val snagOrTbiHeader = TBIFileHeader.read(reader)
      println(snagOrTbiHeader)
    }
  }
}
