package lunaris.app

import lunaris.data.DataSources
import lunaris.io.{ByteBufferReader, ByteBufferRefiller, ResourceConfig}
import lunaris.io.bgz.BGZBlock
import org.broadinstitute.yootilz.core.snag.Snag

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
      var snags: Seq[Snag] = Seq.empty
      var string: String  = ""
      for(_ <- 0 until 10000) {
        reader.readBytes(args(0).toInt) match {
          case Left(snag) =>
            println(snag.message)
            println(snag.report)
          case Right(bytes) =>
            string = new String(bytes)
        }
      }
      println(string)
      snags.foreach { snag =>
        println(snag.message)
        println(snag.report)
      }
    }
  }
}
