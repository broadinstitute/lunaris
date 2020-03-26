package lunaris.io.bgz

import java.nio.ByteBuffer
import java.nio.channels.ReadableByteChannel

class BGZReader(val readChannel: ReadableByteChannel) {
  val bufferSize: Int = 100
  val buffer: ByteBuffer = ByteBuffer.allocate(bufferSize)

  val bytesRead: Int = readChannel.read(buffer)
  println(s"Read $bytesRead bytes.")
  buffer.flip()
  val bytes = new Array[Byte](100)
  buffer.get(bytes)
  println("The bytes:")
  println(bytes.mkString(" "))
  println("These were the bytes.")
}

object BGZReader {
  def apply(readChannel: ReadableByteChannel): BGZReader = new BGZReader(readChannel)
}