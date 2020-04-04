package lunaris.io.query

import java.nio.channels.ReadableByteChannel

import lunaris.data.DataSourceWithIndex
import lunaris.genomics.Regions
import lunaris.io.ByteBufferRefiller
import lunaris.stream.Record
import lunaris.utils.Eitherator

object RecordExtractor {

  def extract(dataReadChannel: ReadableByteChannel, indexReadChannel: ReadableByteChannel,
              regions: Regions): Eitherator[Record] = {
    val indexBufferSize = 100000
    val indexRefiller = ByteBufferRefiller.bgunzip(indexReadChannel, indexBufferSize)

    ???
  }

}
