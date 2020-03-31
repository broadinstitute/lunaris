package lunaris.io.bgz

import java.io.ByteArrayInputStream
import java.util.zip.GZIPInputStream

import org.broadinstitute.yootilz.core.snag.Snag

import scala.util.control.NonFatal

case class BGZUnzippedData(bytes: Array[Byte])

object BGZUnzippedData {
  def read(gzippedData: Array[Byte], zippedSize: Int): Either[Snag, BGZUnzippedData] = {
    try {
      val unzippedInputStream = new ByteArrayInputStream(gzippedData, 0, zippedSize)
      val gzipInputStream = new GZIPInputStream(unzippedInputStream)
      val unzippedBytes = gzipInputStream.readAllBytes()
      Right(BGZUnzippedData(unzippedBytes))
    } catch {
      case NonFatal(ex) => Left(Snag(ex))
    }
  }
}
