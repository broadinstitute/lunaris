package lunaris.io.query

import lunaris.data.DataSourceWithIndex
import lunaris.genomics.Region
import lunaris.io.tbi.{TBIChunk, TBIFileReader}
import lunaris.io.{ByteBufferReader, ByteBufferRefiller, ResourceConfig}
import lunaris.stream.{Header, Record}
import lunaris.utils.{DebugUtils, Eitherator}
import org.broadinstitute.yootilz.core.snag.Snag

object RecordExtractor {

  def extract(dataSourceWithIndex: DataSourceWithIndex,
              regionsBySequence: Map[String, Seq[Region]]): Eitherator[Record] = {
    dataSourceWithIndex.index.newReadChannelDisposable(ResourceConfig.empty).useUp { indexReadChannel =>
      println("Now extracting records")
      val bufferSize = 10000
      val indexReader = new ByteBufferReader(ByteBufferRefiller.bgunzip(indexReadChannel, bufferSize))
      val headerAndChunksPlusEitherator =
        TBIFileReader.readChunksWithSequenceAndRegions(indexReader, regionsBySequence)
      headerAndChunksPlusEitherator.snagOrHeader match {
        case Left(snag) => Eitherator.forSnag(snag)
        case Right(indexHeader) =>
          dataSourceWithIndex.dataSource.newReadChannelDisposable(ResourceConfig.empty).useUp { dataReadChannel =>
            val dataBufferSize = 65536
            val dataRefiller = ByteBufferRefiller.bgunzip(dataReadChannel, dataBufferSize)
            val dataReader = ByteBufferReader(dataRefiller)
            dataRefiller.currentChunk = TBIChunk.wholeFile
            val snagOrHeader = for {
              line <- dataReader.readLine()
              header <- Header.parse(line, indexHeader.col_seq, indexHeader.col_beg, indexHeader.col_end)
            } yield header
            snagOrHeader match {
              case Left(snag) => Eitherator.forSnag(snag)
              case Right(header) =>
                DebugUtils.println(header)
                headerAndChunksPlusEitherator.chunksPlusEter.flatMap { chunkWithSequenceAndRegions =>
                  dataRefiller.currentChunk = chunkWithSequenceAndRegions.chunk
                  val lineEitherator = Eitherator.fromGenerator(!dataRefiller.isAtChunkEnd)(dataReader.readLine())
                  lineEitherator.snagOrForeach { snag =>
                    println("Problem reading line!")
                    println(snag.message)
                    println(snag.report)
                  } { line =>
                    println(line)
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
          }
          println("Done extracting records!")
          Eitherator.empty
      }
    }
  }
}
