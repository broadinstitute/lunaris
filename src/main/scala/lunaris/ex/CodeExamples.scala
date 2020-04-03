package lunaris.ex

import lunaris.data.DataSources
import lunaris.io.{ByteBufferReader, ByteBufferRefiller, ResourceConfig}
import lunaris.io.bgz.BGZBlock
import org.broadinstitute.yootilz.core.snag.Snag

object CodeExamples {
  def readingChunksOfUnzippedData(): Unit = {
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
      val nChunks = 1000
      val chunkSize = 1000
      for(_ <- 0 until nChunks) {
        reader.readBytes(chunkSize) match {
          case Left(snag) =>
            println(snag.message)
            println(snag.report)
            snags :+= snag
          case Right(bytes) =>
            string += new String(bytes)
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
