package lunaris.io.bgz

import java.nio.channels.ReadableByteChannel
import java.nio.{ByteBuffer, ByteOrder}

import org.broadinstitute.yootilz.core.snag.Snag

class BGZReader(val readChannel: ReadableByteChannel) {
  val bufferSize: Int = 1000
  val buffer: ByteBuffer = ByteBuffer.allocate(bufferSize)
  buffer.order(ByteOrder.LITTLE_ENDIAN)

  val bytesRead: Int = readChannel.read(buffer)
  println(s"Read $bytesRead bytes.")
  buffer.flip()
  val either: Either[Snag, BGZHeader] = BGZHeader.read(buffer)
  println("The either:")
  println(either)
  println("This was the either.")
}

object BGZReader {
  def apply(readChannel: ReadableByteChannel): BGZReader = new BGZReader(readChannel)
}