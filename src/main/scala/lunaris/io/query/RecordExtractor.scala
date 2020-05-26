package lunaris.io.query

import lunaris.data.BlockGzippedWithIndex
import lunaris.genomics.Region
import lunaris.io.tbi.{TBIChunk, TBIFileReader}
import lunaris.io.{ByteBufferReader, ByteBufferRefiller, Disposable, ResourceConfig}
import lunaris.recipes.values.{LunValue, ObjectStream}
import lunaris.streams.{Header, Record, RecordProcessor}
import lunaris.utils.{DebugUtils, Eitherator}
import org.broadinstitute.yootilz.core.snag.Snag

object RecordExtractor {

  case class HeaderAndRecordEtor(header: Header, meta: ObjectStream.Meta, recordEtor: Eitherator[Record])

  def extractRecords(dataSourceWithIndex: BlockGzippedWithIndex,
                     regionsBySequence: Map[String, Seq[Region]],
                     recordProcessor: RecordProcessor[Record],
                     resourceConfig: ResourceConfig): Disposable[Either[Snag, HeaderAndRecordEtor]] = {
    dataSourceWithIndex.index.newReadChannelDisposable(resourceConfig).flatMap { indexReadChannel =>
      val bufferSize = 10000
      val indexReader = new ByteBufferReader(ByteBufferRefiller.bgunzip(indexReadChannel, bufferSize))
      val headerAndChunksPlusEitherator =
        TBIFileReader.readChunksWithSequenceAndRegions(indexReader, regionsBySequence)
      headerAndChunksPlusEitherator.snagOrHeader match {
        case Left(snag) => Disposable(Left(snag))(Disposable.Disposer.Noop)
        case Right(indexHeader) =>
          dataSourceWithIndex.data.newReadChannelDisposable(resourceConfig).map { dataReadChannel =>
            val dataBufferSize = 65536
            val dataRefiller = ByteBufferRefiller.bgunzip(dataReadChannel, dataBufferSize)
            val dataReader = ByteBufferReader(dataRefiller)
            dataRefiller.currentChunk = TBIChunk.wholeFile
            val snagOrHeader = for {
              line <- dataReader.readLine()
              header <- Header.parse(line, indexHeader.col_seq, indexHeader.col_beg, indexHeader.col_end)
            } yield header
            snagOrHeader match {
              case Left(snag) => Left(snag)
              case Right(header) =>
                val recordsEtor = headerAndChunksPlusEitherator.chunksPlusEter.flatMap { chunkWithSequenceAndRegions =>
                  dataRefiller.currentChunk = chunkWithSequenceAndRegions.chunk
                  Record.newEitherator(dataReader, header, recordProcessor).filter { record =>
                    val sequence = chunkWithSequenceAndRegions.name
                    val regions = chunkWithSequenceAndRegions.regions
                    record.locus.chrom == sequence && regions.exists(_.overlaps(record.locus.region))
                  }
                }
                Right(HeaderAndRecordEtor(header, ObjectStream.Meta(indexHeader.names), recordsEtor))
            }
          }
      }
    }
  }
}
