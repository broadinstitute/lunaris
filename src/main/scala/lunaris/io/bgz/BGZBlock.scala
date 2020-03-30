package lunaris.io.bgz

import java.nio.{ByteBuffer, ByteOrder}
import java.nio.channels.ReadableByteChannel

import org.broadinstitute.yootilz.core.snag.Snag

case class BGZBlock(header: BGZHeader, footer: BGZFooter) {

}

object BGZBlock {
  val maxBlockSize: Int = 65536  //  Math.pow(2, 16).toInt, per BGZF specs

  def read(readChannel: ReadableByteChannel): Either[Snag, BGZBlock] = {
    val buffer: ByteBuffer = ByteBuffer.allocate(maxBlockSize)
    buffer.order(ByteOrder.LITTLE_ENDIAN)
    val bytesRead: Int = readChannel.read(buffer)
    println(s"Read $bytesRead bytes.")
    buffer.flip()
    val snagOrHeader: Either[Snag, BGZHeader] = BGZHeader.read(buffer)
    println("The snagOrHeader:")
    println(snagOrHeader)
    println("This was the snagOrHeader.")
    for {
      header <- snagOrHeader
      footer <- BGZFooter.read(buffer, header)
    } yield BGZBlock(header, footer)
  }
}