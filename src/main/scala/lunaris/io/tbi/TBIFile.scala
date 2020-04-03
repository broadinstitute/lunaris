package lunaris.io.tbi

import java.nio.ByteOrder

import lunaris.io.ByteBufferReader
import org.broadinstitute.yootilz.core.snag.Snag

case class TBIFile(header: TBIFileHeader)

object TBIFile {
  def read(reader: ByteBufferReader): Either[Snag, TBIFile] = {
    reader.refiller.buffer.order(ByteOrder.LITTLE_ENDIAN)
    for {
      header <- TBIFileHeader.read(reader)
    } yield TBIFile(header)
  }
}