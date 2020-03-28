package lunaris.io

import java.nio.{BufferUnderflowException, ByteBuffer}

import lunaris.io.IntegersIO.{UnsignedByte, UnsignedInt}
import org.broadinstitute.yootilz.core.snag.Snag

class ByteBufferReader(val buffer: ByteBuffer) {

  def underflowExceptionToSnagLeft[T](ex: BufferUnderflowException, fieldName: String): Left[Snag, T] =
    Left(Snag(s"Could not read field $fieldName", Snag(ex)))

  def readField[T](name: String, read: ByteBuffer => T): Either[Snag, T] = {
    try {
      Right(read(buffer))
    } catch {
      case ex: BufferUnderflowException => underflowExceptionToSnagLeft(ex, name)
    }
  }

  def readFieldAssert[T](name: String, read: ByteBuffer => T, expected: T): Either[Snag, T] = {
    try {
      val value = read(buffer)
      if(value == expected) {
        Right(value)
      } else {
        Left(Snag(s"Field $name is $value, but should be $expected."))
      }
    } catch {
      case ex: BufferUnderflowException => underflowExceptionToSnagLeft(ex, name)
    }
  }

  def readUnsignedByteField(name: String): Either[Snag, UnsignedByte] =
    readField(name, buffer => UnsignedByte(buffer.get()))

  def readUnsignedByteFieldAssert(name: String, expected: UnsignedByte): Either[Snag, UnsignedByte] =
    readFieldAssert(name, buffer => UnsignedByte(buffer.get()), expected)

  def readUnsignedIntField(name: String): Either[Snag, UnsignedInt] =
    readField(name, buffer => UnsignedInt(buffer.getInt))

  def readUnsignedIntFieldAssert(name: String, expected: UnsignedInt): Either[Snag, UnsignedInt] =
    readFieldAssert(name, buffer => UnsignedInt(buffer.getInt()), expected)
}
