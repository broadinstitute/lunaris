package lunaris.io.query

import lunaris.data.DataSourceWithIndex
import lunaris.genomics.Region
import lunaris.io.tbi.{TBIChunk, TBIFileReader}
import lunaris.io.{ByteBufferReader, ByteBufferRefiller, ResourceConfig}
import lunaris.stream.{Header, Record}
import lunaris.utils.{DebugUtils, Eitherator}
import org.broadinstitute.yootilz.core.snag.Snag

object RecordExtractor {

  type ParsedRecordHandler = Either[Snag, Record] => Either[Snag, Option[Record]]

  def extract(dataSourceWithIndex: DataSourceWithIndex,
              regionsBySequence: Map[String, Seq[Region]],
              parsedRecordHandler: ParsedRecordHandler): Eitherator[Record] = {
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
                  lineEitherator.process { line =>

                    ???
                  }
                  Eitherator.singleValue(chunkWithSequenceAndRegions)
                }
            }
          }
          println("Done extracting records!")
          Eitherator.empty
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
