package lunaris.io.query

import lunaris.data.DataSourceWithIndex
import lunaris.genomics.Region
import lunaris.io.tbi.{TBIChunk, TBIFileReader}
import lunaris.io.{ByteBufferReader, ByteBufferRefiller, ResourceConfig}
import lunaris.stream.Record
import lunaris.utils.Eitherator

object RecordExtractor {

  def extract(dataSourceWithIndex: DataSourceWithIndex,
              regionsBySequence: Map[String, Seq[Region]]): Eitherator[Record] = {
    dataSourceWithIndex.index.newReadChannelDisposable(ResourceConfig.empty).useUp { indexReadChannel =>
      println("Now extracting records")
      val bufferSize = 10000
      val indexReader = new ByteBufferReader(ByteBufferRefiller.bgunzip(indexReadChannel, bufferSize))
      val indexEitherator = TBIFileReader.readChunksForSequence(indexReader, regionsBySequence)
      dataSourceWithIndex.dataSource.newReadChannelDisposable(ResourceConfig.empty).useUp { dataReadChannel =>
        var keepGoing: Boolean = true
        while (keepGoing) {
          indexEitherator.next() match {
            case Left(snag) =>
              println("Problem!")
              println(snag.message)
              println(snag.report)
              keepGoing = false
            case Right(None) =>
              println("Done!")
              keepGoing = false
            case Right(Some(chunksForSequence)) =>
              val chunks = TBIChunk.consolidateChunksByRegion(chunksForSequence.chunksByRegion)
              println(chunksForSequence.name + " has chunks " + chunks)


          }
        }
      }
      println("Done extracting records!")
      Eitherator.empty
    }
  }
}
