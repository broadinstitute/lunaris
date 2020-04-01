package lunaris.io.bgz

import lunaris.io.ByteBufferReader
import org.broadinstitute.yootilz.core.snag.Snag

case class BGZUnzippedData(bytes: Array[Byte])

object BGZUnzippedData {
  def read(reader: ByteBufferReader, blockSize: Int): Either[Snag, BGZUnzippedData] = {
    reader.unzip(blockSize).map(BGZUnzippedData(_))
  }
}
