package lunaris.app

import lunaris.data.DataSources
import lunaris.io.{ByteBufferReader, ByteBufferRefiller, ResourceConfig}
import lunaris.io.bgz.BGZBlock

object LunarisApp {
  def main(args: Array[String]): Unit = {
    val usingLocalSimulatedDataOnOliversLaptop: Boolean = true
    val dataSourceWithIndex = if (usingLocalSimulatedDataOnOliversLaptop)
      DataSources.simDataOnOliversOldLaptop
    else
      DataSources.simDataOnTerra
    val bufferSize = 100000
    dataSourceWithIndex.dataSource.newReadChannelDisposable(ResourceConfig.empty).useUp { readChannel =>
      val refiller = ByteBufferRefiller(BGZBlock.newBlockEitherator(readChannel).map(_.unzippedData.bytes), bufferSize)
      val reader = new ByteBufferReader(refiller)
//      println(new String(reader.readBytes(10000))
    }
  }
}
