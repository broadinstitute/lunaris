package lunaris.io.bgz

import lunaris.io.IntegersIO.{UnsignedByte, UnsignedInt, UnsignedShort}
import lunaris.io.{ByteBufferReader, ByteBufferRefiller}
import org.broadinstitute.yootilz.core.snag.Snag

case class BGZHeader(mtime: UnsignedInt, xfl: UnsignedByte, os: UnsignedByte, xlen: UnsignedShort,
                     bsize: UnsignedShort) {
  def blockSize: Int = bsize.toPositiveInt + 1
}

object BGZHeader {
  def read(refiller: ByteBufferRefiller): Either[Snag, BGZHeader] = {
    val reader = new ByteBufferReader(refiller)
    for {
      _ <- reader.readUnsignedByteFieldAssert("id1", UnsignedByte(31))
      _ <- reader.readUnsignedByteFieldAssert("id2", UnsignedByte(139.toByte))
      _ <- reader.readUnsignedByteFieldAssert("cm", UnsignedByte(8))
      _ <- reader.readUnsignedByteFieldAssert("flg", UnsignedByte(4))
      mtime <- reader.readUnsignedIntField("mtime")
      xfl <- reader.readUnsignedByteField("xfl")
      os <- reader.readUnsignedByteField("os")
      xlen <- reader.readUnsignedShortField("xlen")
      xlenInt = xlen.toPositiveInt
      _ <- if(xlenInt < 6) Left(Snag(s"xlen needs to be at least 6, but is $xlenInt")) else Right(())
      _ <- reader.readUnsignedByteFieldAssert("si1", UnsignedByte(66))
      _ <- reader.readUnsignedByteFieldAssert("si2", UnsignedByte(67))
      _ <- reader.readUnsignedShortFieldAssert("slen", UnsignedShort(2))
      bsize <- reader.readUnsignedShortField("bsize")
    } yield BGZHeader(mtime, xfl, os, xlen, bsize)
  }
}
