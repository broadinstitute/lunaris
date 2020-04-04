package lunaris.io.query

import lunaris.data.DataSourceWithIndex
import lunaris.genomics.Regions
import lunaris.io.tbi.TBIFileHeader
import lunaris.io.{ByteBufferReader, ByteBufferRefiller, ResourceConfig}
import lunaris.stream.Record
import lunaris.utils.Eitherator

object RecordExtractor {

  def extract(dataSourceWithIndex: DataSourceWithIndex, regions: Regions): Eitherator[Record] = {
    dataSourceWithIndex.index.newReadChannelDisposable(ResourceConfig.empty).useUp { indexReadChannel =>
      val bufferSize = 10000
      val indexReader = new ByteBufferReader(ByteBufferRefiller.bgunzip(indexReadChannel, bufferSize))
      val snagOrTbiHeader = TBIFileHeader.read(indexReader)
      println(snagOrTbiHeader)
    }
    Eitherator.empty
  }

}
