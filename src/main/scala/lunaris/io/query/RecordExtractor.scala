package lunaris.io.query

import lunaris.data.DataSourceWithIndex
import lunaris.genomics.Regions
import lunaris.io.tbi.{TBIFileHeader, TBIFileReader}
import lunaris.io.{ByteBufferReader, ByteBufferRefiller, ResourceConfig}
import lunaris.stream.Record
import lunaris.utils.Eitherator
import org.broadinstitute.yootilz.core.snag.Snag

object RecordExtractor {

  def extract(dataSourceWithIndex: DataSourceWithIndex, regions: Regions): Eitherator[Record] = {
    dataSourceWithIndex.index.newReadChannelDisposable(ResourceConfig.empty).useUp { indexReadChannel =>
      println("Now extracting records")
      val bufferSize = 10000
      val indexReader = new ByteBufferReader(ByteBufferRefiller.bgunzip(indexReadChannel, bufferSize))
      val tbiConsumer = new RecordTbiConsumer()
      TBIFileReader.readFile(indexReader, tbiConsumer)
      println("Done extracting records!")
      Eitherator.empty
    }
  }

  class RecordTbiConsumer extends TBIFileReader.TBIConsumer {
    override def consumeHeader(header: TBIFileHeader): Unit = {
      println("TBI file header: " + header)
    }

    override def startSequenceIndex(name: String): Unit = {
      println("Starting sequence index for " + name)
    }

    override def consumeNBins(nBins: Int): Unit = {
      println("Number of bins is: " + nBins)
    }

    override def consumeSnag(snag: Snag): Unit = {
      println("There was a problem reading the TBI file!")
      println(snag.message)
      println(snag.report)
    }
  }

}
