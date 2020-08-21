package lunaris.io.query

import lunaris.data.BlockGzippedWithIndex
import lunaris.genomics.Region
import lunaris.io.tbi.{TBIChunk, TBIFileReader}
import lunaris.io.{ByteBufferReader, ByteBufferRefiller, Disposable, ResourceConfig}
import lunaris.recipes.values.RecordStreamWithMeta
import lunaris.streams.{TsvHeader, TsvRecord, RecordProcessor}
import lunaris.utils.Eitherator
import org.broadinstitute.yootilz.core.snag.Snag

object RecordExtractor {

  case class HeaderAndRecordEtor(header: TsvHeader, meta: RecordStreamWithMeta.Meta, recordEtor: Eitherator[TsvRecord])

  def extractRecords(dataSourceWithIndex: BlockGzippedWithIndex,
                     regionsBySequence: Map[String, Seq[Region]],
                     idField: String,
                     recordProcessor: RecordProcessor[TsvRecord],
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
            val snagOrHeader =
              TsvHeader.parseLines(indexHeader.col_seq, indexHeader.col_beg, indexHeader.col_end)(dataReader.readLine)
            snagOrHeader match {
              case Left(snag) => Left(snag)
              case Right(header) =>
                val recordsEtor = headerAndChunksPlusEitherator.chunksPlusEter.flatMap { chunkWithSequenceAndRegions =>
                  dataRefiller.currentChunk = chunkWithSequenceAndRegions.chunk
                  TsvRecord.newEitherator(dataReader, header, recordProcessor).filter { record =>
                    val sequence = chunkWithSequenceAndRegions.name
                    val regions = chunkWithSequenceAndRegions.regions
                    record.locus.chrom == sequence && regions.exists(_.overlaps(record.locus.region))
                  }
                }
                header.toLunRecordType(idField).map { recordType =>
                  HeaderAndRecordEtor(header, RecordStreamWithMeta.Meta(recordType, indexHeader.names), recordsEtor)
                }
            }
          }
      }
    }
  }
}
