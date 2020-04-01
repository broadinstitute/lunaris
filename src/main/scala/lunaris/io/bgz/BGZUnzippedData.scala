package lunaris.io.bgz

import java.io.ByteArrayInputStream
import java.util.zip.GZIPInputStream

import lunaris.io.ByteBufferRefiller
import org.broadinstitute.yootilz.core.snag.Snag

import scala.util.control.NonFatal

case class BGZUnzippedData(bytes: Array[Byte])

object BGZUnzippedData {
  def read(refiller: ByteBufferRefiller, zippedSize: Int): Either[Snag, BGZUnzippedData] = {
    try {
      val unzippedInputStream = new ByteArrayInputStream(refiller.buffer.array(), 0, zippedSize)
      val gzipInputStream = new GZIPInputStream(unzippedInputStream)
      val unzippedBytes = gzipInputStream.readAllBytes()
      Right(BGZUnzippedData(unzippedBytes))
    } catch {
      case NonFatal(ex) => Left(Snag(ex))
    }
  }
}
