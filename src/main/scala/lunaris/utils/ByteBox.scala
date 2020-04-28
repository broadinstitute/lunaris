package lunaris.utils

import java.nio.{ByteBuffer, ByteOrder}

class ByteBox(val size: Int) {
  private val bytes: Array[Byte] = new Array[Byte](size)
  val buffer: ByteBuffer = ByteBuffer.wrap(bytes)
  buffer.order(ByteOrder.LITTLE_ENDIAN)
  buffer.flip()

  def remaining: Int = buffer.remaining()

  def writeToBuffer[A](writer: ByteBuffer => A): A = {
    buffer.compact()
    val a = writer(buffer)
    buffer.flip()
    a
  }

  def readFromBuffer[A](reader: ByteBuffer => A): A = reader(buffer)

  def skip(nBytes: Int): Unit = {
    buffer.position(buffer.position() + nBytes)
  }

  def clear(): Unit = {
    buffer.clear()
    buffer.flip()
  }

  def readAndReset[A](reader: => A): A = {
    // TODO making sure enough bytes are available
    buffer.mark()
    val a = reader
    buffer.reset()
    a
  }
}

object ByteBox {
  def apply(size: Int): ByteBox = new ByteBox(size)
}