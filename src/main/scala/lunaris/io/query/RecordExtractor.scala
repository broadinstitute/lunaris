package lunaris.io.query

import lunaris.data.DataSourceWithIndex
import lunaris.genomics.Region
import lunaris.io.tbi.TBIFileReader.TBIChunkWithSequenceAndRegions
import lunaris.io.tbi.{TBIChunk, TBIFileReader}
import lunaris.io.{ByteBufferReader, ByteBufferRefiller, ResourceConfig}
import lunaris.stream.Record
import lunaris.utils.Eitherator
import org.broadinstitute.yootilz.core.snag.Snag

object RecordExtractor {

  def extract(dataSourceWithIndex: DataSourceWithIndex,
              regionsBySequence: Map[String, Seq[Region]]): Eitherator[Record] = {
    dataSourceWithIndex.index.newReadChannelDisposable(ResourceConfig.empty).useUp { indexReadChannel =>
      println("Now extracting records")
      val bufferSize = 10000
      val indexReader = new ByteBufferReader(ByteBufferRefiller.bgunzip(indexReadChannel, bufferSize))
//      val indexEitherator = TBIFileReader.readChunksForSequence(indexReader, regionsBySequence)
      val chunksPlusEitherator =
        TBIFileReader.readChunksWithSequenceAndRegions(indexReader, regionsBySequence)
      dataSourceWithIndex.dataSource.newReadChannelDisposable(ResourceConfig.empty).useUp { dataReadChannel =>
        val dataBufferSize = 65536
        val dataRefiller = ByteBufferRefiller.bgunzip(dataReadChannel, dataBufferSize)
        val dataReader = ByteBufferReader(dataRefiller)
        chunksPlusEitherator.flatMap { chunkWithSequenceAndRegions =>
          dataRefiller.currentChunk = chunkWithSequenceAndRegions.chunk
          var snagOpt: Option[Snag] = None
          while(snagOpt.isEmpty && !dataRefiller.isAtChunkEnd) {
            dataReader.readLine() match {
              case Left(snag) =>
                println("Problem reading line!")
                println(snag.message)
                println(snag.report)
                snagOpt = Some(snag)
              case Right(line) =>
                println("Line:")
                println(line)
            }
          }
          Eitherator.singleton(chunkWithSequenceAndRegions)
        }.foreach { chunkWithSequenceAndRegions =>
          println(chunkWithSequenceAndRegions)
        } match {
          case Left(snag) =>
            println("Problem reading chunks!")
            println(snag.message)
            println(snag.report)
          case _ => ()
        }
      }
      println("Done extracting records!")
      Eitherator.empty
    }
  }
}
