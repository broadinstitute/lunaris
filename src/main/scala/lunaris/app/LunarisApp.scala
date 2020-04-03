package lunaris.app

import lunaris.data.DataSources
import lunaris.io.bgz.BGZBlock
import lunaris.io.tbi.TBIFile
import lunaris.io.{ByteBufferReader, ByteBufferRefiller, ResourceConfig}
import org.broadinstitute.yootilz.core.snag.Snag

object LunarisApp {
  def main(args: Array[String]): Unit = {
    val usingLocalSimulatedDataOnOliversLaptop: Boolean = true
    val dataSourceWithIndex = if (usingLocalSimulatedDataOnOliversLaptop)
      DataSources.simDataOnOliversOldLaptop
    else
      DataSources.simDataOnTerra
    dataSourceWithIndex.index.newReadChannelDisposable(ResourceConfig.empty).useUp { readChannel =>
      val bufferSize = 10000
      val refiller = ByteBufferRefiller(BGZBlock.newBlockEitherator(readChannel).map(_.unzippedData.bytes), bufferSize)
      val reader = new ByteBufferReader(refiller)
      val snagOrTbiFile = TBIFile.read(reader)
      println(snagOrTbiFile)
    }
  }
}
