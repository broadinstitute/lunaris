package lunaris.io

import java.nio.{BufferUnderflowException, ByteBuffer}

import lunaris.io.IntegersIO.{UnsignedByte, UnsignedInt, UnsignedShort}
import org.broadinstitute.yootilz.core.snag.Snag

class ByteBufferReader(val refiller: ByteBufferRefiller) {

  def underflowExceptionToSnagLeft[T](ex: BufferUnderflowException, fieldName: String): Left[Snag, T] =
    Left(Snag(s"Could not read field $fieldName", Snag(ex)))

  def readField[T](name: String, read: ByteBuffer => T, nBytesNeeded: Int): Either[Snag, T] = {
    try {
      refiller.read(read, nBytesNeeded)
    } catch {
      case ex: BufferUnderflowException => underflowExceptionToSnagLeft(ex, name)
    }
  }

  def readFieldAssert[T](name: String, read: ByteBuffer => T, expected: T, nBytesNeeded: Int): Either[Snag, T] = {
    for {
      value <- refiller.read(read, nBytesNeeded)
      valueAgain <-
        if (value == expected) {
          Right(value)
        } else {
          Left(Snag(s"Field $name is $value, but should be $expected."))
        }
    } yield valueAgain
  }

  def readUnsignedByteField(name: String): Either[Snag, UnsignedByte] =
    readField(name, buffer => UnsignedByte(buffer.get()), 1)

  def readUnsignedByteFieldAssert(name: String, expected: UnsignedByte): Either[Snag, UnsignedByte] =
    readFieldAssert(name, buffer => UnsignedByte(buffer.get()), expected, 1)

  def readUnsignedShortField(name: String): Either[Snag, UnsignedShort] =
    readField(name, buffer => UnsignedShort(buffer.getShort()), 2)

  def readUnsignedShortFieldAssert(name: String, expected: UnsignedShort): Either[Snag, UnsignedShort] =
    readFieldAssert(name, buffer => UnsignedShort(buffer.getShort()), expected, 2)

  def readUnsignedIntField(name: String): Either[Snag, UnsignedInt] =
    readField(name, buffer => UnsignedInt(buffer.getInt()), 4)

  def readUnsignedIntFieldAssert(name: String, expected: UnsignedInt): Either[Snag, UnsignedInt] =
    readFieldAssert(name, buffer => UnsignedInt(buffer.getInt()), expected, 4)
}
