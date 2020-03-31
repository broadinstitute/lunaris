package lunaris.io.bgz

import java.nio.ByteBuffer

import lunaris.io.{ByteBufferReader, ByteBufferRefiller}
import lunaris.io.IntegersIO.UnsignedInt
import org.broadinstitute.yootilz.core.snag.Snag

case class BGZFooter(crc32: UnsignedInt, isize: UnsignedInt) {
  def unzippedDataSize: Int = isize.toPositiveIntOpt.get  // no larger than 65536 according to BGZF specs
}

object BGZFooter {
  val nFooterBytes = 8

  def read(refiller: ByteBufferRefiller, header: BGZHeader): Either[Snag, BGZFooter] = read(refiller, header.blockSize)

  def read(refiller: ByteBufferRefiller, blockSize: Int): Either[Snag, BGZFooter] = {
    refiller.buffer.position(blockSize - nFooterBytes)
    val reader = new ByteBufferReader(refiller)
    for {
      crc32 <- reader.readUnsignedIntField("crc32")
      isize <- reader.readUnsignedIntField("isize")
      _ <- if(isize.toPositiveIntOpt.isEmpty) {
        Left(Snag(s"isize should be less than 65536, but is $isize."))
      } else {
        Right(())
      }
    } yield BGZFooter(crc32, isize)
  }
}