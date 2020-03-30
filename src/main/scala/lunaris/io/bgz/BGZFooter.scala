package lunaris.io.bgz

import java.nio.ByteBuffer

import lunaris.io.ByteBufferReader
import lunaris.io.IntegersIO.UnsignedInt
import org.broadinstitute.yootilz.core.snag.Snag

case class BGZFooter(crc32: UnsignedInt, isize: UnsignedInt)

object BGZFooter {
  val nFooterBytes = 8

  def read(buffer: ByteBuffer, header: BGZHeader): Either[Snag, BGZFooter] = read(buffer, header.blockSize)

  def read(buffer: ByteBuffer, blockSize: Int): Either[Snag, BGZFooter] = {
    buffer.position(blockSize - nFooterBytes)
    val reader = new ByteBufferReader(buffer)
    for {
      crc32 <- reader.readUnsignedIntField("crc32")
      isize <- reader.readUnsignedIntField("isize")
    } yield BGZFooter(crc32, isize)
  }
}