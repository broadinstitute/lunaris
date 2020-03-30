package lunaris.io.bgz

import java.nio.{ByteBuffer, ByteOrder}
import java.nio.channels.ReadableByteChannel

import org.broadinstitute.yootilz.core.snag.Snag

case class BGZBlock(header: BGZHeader, footer: BGZFooter, unzippedData: BGZUnzippedData) {

}

object BGZBlock {
  val maxBlockSize: Int = 65536  //  Math.pow(2, 16).toInt, per BGZF specs

  def read(readChannel: ReadableByteChannel): Either[Snag, BGZBlock] = {
    val bytes = new Array[Byte](maxBlockSize)
    val buffer: ByteBuffer = ByteBuffer.wrap(bytes)
    buffer.order(ByteOrder.LITTLE_ENDIAN)
    val bytesRead: Int = readChannel.read(buffer)
    println(s"Bytes read into buffer: $bytesRead")
    buffer.flip()
    for {
      header <- BGZHeader.read(buffer)
      footer <- BGZFooter.read(buffer, header)
      unzippedData <- BGZUnzippedData.read(bytes, header.blockSize, footer.unzippedDataSize)
    } yield BGZBlock(header, footer, unzippedData)
  }
}