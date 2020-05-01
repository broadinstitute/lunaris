package lunaris.io.query

import lunaris.data.DataSourceWithIndex
import lunaris.genomics.Region
import lunaris.io.tbi.{TBIChunk, TBIFileReader}
import lunaris.io.{ByteBufferReader, ByteBufferRefiller, Disposable, ResourceConfig}
import lunaris.stream.{Header, Record}
import lunaris.utils.{DebugUtils, Eitherator}
import org.broadinstitute.yootilz.core.snag.Snag

object RecordExtractor {

  type ParsedRecordHandler = Either[Snag, Record] => Either[Snag, Option[Record]]

  case class HeaderAndRecordEtor(header: Header, recordEtor: Eitherator[Record])

  def extract(dataSourceWithIndex: DataSourceWithIndex,
              regionsBySequence: Map[String, Seq[Region]],
              parsedRecordHandler: ParsedRecordHandler): Disposable[Either[Snag, HeaderAndRecordEtor]] = {
    dataSourceWithIndex.index.newReadChannelDisposable(ResourceConfig.empty).flatMap { indexReadChannel =>
      val bufferSize = 10000
      val indexReader = new ByteBufferReader(ByteBufferRefiller.bgunzip(indexReadChannel, bufferSize))
      val headerAndChunksPlusEitherator =
        TBIFileReader.readChunksWithSequenceAndRegions(indexReader, regionsBySequence)
      headerAndChunksPlusEitherator.snagOrHeader match {
        case Left(snag) => Disposable(Left(snag))(Disposable.Disposer.Noop)
        case Right(indexHeader) =>
          dataSourceWithIndex.dataSource.newReadChannelDisposable(ResourceConfig.empty).map { dataReadChannel =>
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
                  val lineEitherator =
                    Eitherator.fromGenerator(!dataRefiller.isAtChunkEnd)(dataReader.readLine())
                  lineEitherator.process { line =>
                    parsedRecordHandler(Record.parse(line, header))
                  }.filter { record =>
                    val sequence = chunkWithSequenceAndRegions.name
                    val regions = chunkWithSequenceAndRegions.regions
                    record.seq == sequence && regions.exists(_.overlaps(record.region))
                  }
                }
                Right(HeaderAndRecordEtor(header, recordsEtor))
            }
          }
      }
    }
  }

  object ParsedRecordHandler {
    val failOnFaultyRecord: ParsedRecordHandler = {
      case Left(snag) => Left(snag)
      case Right(record) => Right(Some(record))
    }
    val ignoreFaultyRecords: ParsedRecordHandler = {
      case Left(snag) => Right(None)
      case Right(record) => Right(Some(record))
    }

    def newFaultyRecordsLogger(): ParsedRecordHandler = new ParsedRecordHandler {
      var snags: Seq[Snag] = Vector.empty

      override def apply(snagOrRecord: Either[Snag, Record]): Either[Snag, Option[Record]] = {
        snagOrRecord match {
          case Left(snag) =>
            snags :+= snag
            Right(None)
          case Right(record) =>
            Right(Some(record))
        }
      }
    }
  }

}
