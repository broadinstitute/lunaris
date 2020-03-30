package lunaris.io.bgz

import java.io.ByteArrayInputStream
import java.util.zip.GZIPInputStream

import org.broadinstitute.yootilz.core.snag.Snag

import scala.util.control.NonFatal

case class BGZUnzippedData(bytes: Array[Byte])

object BGZUnzippedData {
  def read(gzippedData: Array[Byte], zippedSize: Int, unzippedSize: Int): Either[Snag, BGZUnzippedData] = {
    try {
      val unzippedInputStream = new ByteArrayInputStream(gzippedData, 0, unzippedSize)
      val gzipInputStream = new GZIPInputStream(unzippedInputStream)
      val unzippedBytes = new Array[Byte](unzippedSize)
      gzipInputStream.read(unzippedBytes)
      Right(BGZUnzippedData(unzippedBytes))
    } catch {
      case NonFatal(ex) => Left(Snag(ex))
    }
  }
}
