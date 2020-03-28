package lunaris.io.bgz

import java.nio.{ByteBuffer, ByteOrder}

import lunaris.io.ByteBufferReader
import lunaris.io.IntegersIO.{UnsignedByte, UnsignedInt}
import org.broadinstitute.yootilz.core.snag.Snag

case class BGZHeader(mtime: UnsignedInt)

object BGZHeader {
  def read(buffer: ByteBuffer): Either[Snag, BGZHeader] = {
    buffer.order(ByteOrder.LITTLE_ENDIAN)
    val reader = new ByteBufferReader(buffer)
    for {
      _ <- reader.readUnsignedByteFieldAssert("id1", UnsignedByte(31))
      _ <- reader.readUnsignedByteFieldAssert("id2", UnsignedByte(139.toByte))
      _ <- reader.readUnsignedByteFieldAssert("cm", UnsignedByte(8))
      _ <- reader.readUnsignedByteFieldAssert("flg", UnsignedByte(4))
      mtime <- reader.readUnsignedIntField("mtime")
    } yield BGZHeader(mtime)
  }
}
