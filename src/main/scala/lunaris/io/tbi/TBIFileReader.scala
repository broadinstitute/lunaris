package lunaris.io.tbi

import lunaris.io.ByteBufferReader
import org.broadinstitute.yootilz.core.snag.Snag

object TBIFileReader {

  trait TBIConsumer {
    def consumeHeader(header: TBIFileHeader): Unit
    def startSequenceIndex(name: String): Unit
    def consumeNBins(nBins: Int): Unit
    def consumeSnag(snag: Snag): Unit
  }

  def readFile(reader: ByteBufferReader, consumer: TBIConsumer): Unit = {
    TBIFileHeader.read(reader).fold(consumer.consumeSnag, consumer.consumeHeader)
  }

}
