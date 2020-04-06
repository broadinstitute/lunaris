package lunaris.io.query

import lunaris.data.DataSourceWithIndex
import lunaris.genomics.Regions
import lunaris.io.tbi.{TBIFileHeader, TBIFileReader, TbiVirtualFileOffset}
import lunaris.io.{ByteBufferReader, ByteBufferRefiller, IntegersIO, ResourceConfig}
import lunaris.stream.Record
import lunaris.utils.Eitherator
import org.broadinstitute.yootilz.core.snag.Snag

object RecordExtractor {

  def extract(dataSourceWithIndex: DataSourceWithIndex, regions: Regions): Eitherator[Record] = {
    dataSourceWithIndex.index.newReadChannelDisposable(ResourceConfig.empty).useUp { indexReadChannel =>
      println("Now extracting records")
      val bufferSize = 10000
      val indexReader = new ByteBufferReader(ByteBufferRefiller.bgunzip(indexReadChannel, bufferSize))
      val tbiConsumer = new RecordTbiConsumer(new LimitedLogger(50000))
      TBIFileReader.readFile(indexReader, tbiConsumer)
      println("Done extracting records!")
      Eitherator.empty
    }
  }

  class LimitedLogger(limit: Int) extends (String => Unit) {
    var count: Int = 0
    override def apply(line: String): Unit = {
      if(count < limit) {
        println(line)
        count += 1
      }
    }
  }

  class RecordTbiConsumer(logger: String => Unit) extends TBIFileReader.TBIConsumer {
    override def consumeHeader(header: TBIFileHeader): Unit = {
      logger("TBI file header: " + header)
    }

    override def startSequenceIndex(name: String): Unit = {
      logger("Starting sequence index for " + name)
    }

    override def consumeNBins(nBins: Int): Unit = {
      logger("Number of bins is: " + nBins)
    }
    override def consumeBinNumber(bin: IntegersIO.UnsignedInt): Unit = {
      logger("Bin number is " + bin.toPositiveLong.toString)
    }

    override def consumeNChunks(nChunks: Int): Unit = {
      logger("Number of chunks is: " + nChunks)
    }

    override def consumeChunk(chunk: TBIFileReader.Chunk): Unit = {
      logger("Chunk is " + chunk)
    }

    override def consumeNIntervals(nIntervals: Int): Unit = {
      logger("Number of intervals is " + nIntervals)
    }

    override def consumeIntervalOffset(offset: TbiVirtualFileOffset): Unit = {
      logger("Interval offset is : " + offset)
    }

    override def doneWithSequenceIndex(name: String): Unit = {
      logger("Done with sequence index for " + name)
    }

    override def consumeSnag(snag: Snag): Unit = {
      println("There was a problem reading the TBI file!")
      println(snag.message)
      println(snag.report)
    }
  }

}
