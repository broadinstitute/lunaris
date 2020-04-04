package lunaris.io.tbi

import lunaris.io.ByteBufferReader
import org.broadinstitute.yootilz.core.snag.Snag

object TBIFileReader {

  trait TBIConsumer {
    def consumeHeader(header: TBIFileHeader)
    def startSequenceIndex(name: String)
    def consumeNBins(nBins: Int)
    def consumeSnag(snag: Snag)
  }

  def readFile(reader: ByteBufferReader, consumer: TBIConsumer): Unit = {
    TBIFileHeader.read(reader).fold(consumer.consumeSnag, consumer.consumeHeader)

  }

}
