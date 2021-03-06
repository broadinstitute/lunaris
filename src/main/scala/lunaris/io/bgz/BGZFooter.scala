package lunaris.io.bgz

import lunaris.io.ByteBufferReader
import lunaris.io.IntegersIO.UnsignedInt
import org.broadinstitute.yootilz.core.snag.Snag

case class BGZFooter(crc32: UnsignedInt, isize: UnsignedInt) {
  def unzippedDataSize: Int = isize.toPositiveIntOpt.get  // no larger than 65536 according to BGZF specs
}

object BGZFooter {
  val nFooterBytes = 8

  def read(reader: ByteBufferReader): Either[Snag, BGZFooter] = {
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