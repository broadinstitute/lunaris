package lunaris.io.bgz

import java.nio.{ByteBuffer, ByteOrder}

import lunaris.io.IntegersIO.{UnsignedByte, UnsignedInt}
import org.broadinstitute.yootilz.core.snag.Snag

import scala.util.Try

case class BGZHeader(mtime: UnsignedInt)

object BGZHeader {
  def read(buffer: ByteBuffer): Either[Snag, BGZHeader] = {
    Try {
      buffer.order(ByteOrder.LITTLE_ENDIAN)
      var snagOpt: Option[Snag] = None
      val id1 = buffer.get()
      val id1Expected: Byte = 31
      if(id1 != id1Expected) {
        snagOpt = Some(Snag(s"id1 needs to be $id1Expected, but is $id1"))
      }
      val id2 = UnsignedByte(buffer.get())
      val id2Expected: UnsignedByte = UnsignedByte((139 - UnsignedByte.shiftToPositive).toByte)
      if(id2 != id2Expected) {
        snagOpt = Some(Snag(s"id2 needs to be $id2Expected, but is $id2"))
      }
      val cm = buffer.get()
      val cmExpected: Byte = 8
      if(cm != cmExpected) {
        snagOpt = Some(Snag(s"cm needs to be $cmExpected, but is $cm"))
      }
      val flg = buffer.get()
      val flgExpected: Byte = 4
      if(flg != flgExpected) {
        snagOpt = Some(Snag(s"flg needs to be $flgExpected, but is $flg"))
      }
      val mtime = UnsignedInt(buffer.getInt)
      snagOpt.fold[Either[Snag, BGZHeader]](Right(BGZHeader(mtime)))(Left(_))
    }.toEither.fold(throwable => Left(Snag(throwable)), either => either)
  }
}
