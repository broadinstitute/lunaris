package lunaris.utils

import java.nio.ByteBuffer

class ByteBox(val size: Int) {
  private val bytes: Array[Byte] = new Array[Byte](size)
  val buffer: ByteBuffer = ByteBuffer.wrap(bytes)

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
}

object ByteBox {
  def apply(size: Int): ByteBox = new ByteBox(size)
}