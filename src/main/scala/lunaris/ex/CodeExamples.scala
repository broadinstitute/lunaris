package lunaris.ex

import lunaris.data.{DataSourceWithIndex, DataSources}
import lunaris.io.tbi.TBIFileHeader
import lunaris.io.{ByteBufferReader, ByteBufferRefiller, ResourceConfig}
import org.broadinstitute.yootilz.core.snag.Snag

object CodeExamples {
  def getDataSourceWithIndex: DataSourceWithIndex = {
    val usingLocalSimulatedDataOnOliversLaptop: Boolean = true
    if (usingLocalSimulatedDataOnOliversLaptop)
      DataSources.simDataOnOliversOldLaptop
    else
      DataSources.simDataOnTerra
  }

  def readingChunksOfUnzippedData(): Unit = {
    val bufferSize = 100000
    getDataSourceWithIndex.dataSource.newReadChannelDisposable(ResourceConfig.empty).useUp { readChannel =>
      val refiller = ByteBufferRefiller.bgunzip(readChannel, bufferSize)
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

  def readTbiFileHeader(): Unit = {
    getDataSourceWithIndex.index.newReadChannelDisposable(ResourceConfig.empty).useUp { readChannel =>
      val bufferSize = 10000
      val refiller = ByteBufferRefiller.bgunzip(readChannel, bufferSize)
      val reader = new ByteBufferReader(refiller)
      val snagOrTbiHeader = TBIFileHeader.read(reader)
      println(snagOrTbiHeader)
    }
  }
}
