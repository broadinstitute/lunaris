package lunaris.io.query

import lunaris.data.BlockGzippedWithIndex
import lunaris.io.tbi.{TBIChunk, TBIFileHeader}
import lunaris.io.{ByteBufferReader, ByteBufferRefiller, Disposable, ResourceConfig}
import lunaris.streams.TsvHeader
import org.broadinstitute.yootilz.core.snag.Snag

object HeaderExtractor {

  def extractHeader(dataSourceWithIndex: BlockGzippedWithIndex,
                    resourceConfig: ResourceConfig): Disposable[Either[Snag, TsvHeader]] = {
    dataSourceWithIndex.index.newReadChannelDisposable(resourceConfig).flatMap { indexReadChannel =>
      val indexBufferSize = 10000
      val indexReader = new ByteBufferReader(ByteBufferRefiller.bgunzip(indexReadChannel, indexBufferSize))
      TBIFileHeader.read(indexReader) match {
        case Left(snag) => Disposable(Left(snag))(Disposable.Disposer.Noop)
        case Right(indexHeader) =>
          dataSourceWithIndex.data.newReadChannelDisposable(resourceConfig).map { dataReadChannel =>
            val dataBufferSize = 65536
            val dataRefiller = ByteBufferRefiller.bgunzip(dataReadChannel, dataBufferSize)
            val dataReader = ByteBufferReader(dataRefiller)
            dataRefiller.currentChunk = TBIChunk.wholeFile
            TsvHeader.parseLines(indexHeader.col_seq, indexHeader.col_beg, indexHeader.col_end)(dataReader.readLine)
          }
      }
    }
  }
}
