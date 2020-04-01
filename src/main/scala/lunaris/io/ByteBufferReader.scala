package lunaris.io

import java.io.ByteArrayInputStream
import java.nio.{BufferUnderflowException, ByteBuffer}
import java.util.zip.GZIPInputStream

import lunaris.io.IntegersIO.{UnsignedByte, UnsignedInt, UnsignedShort}
import org.broadinstitute.yootilz.core.snag.Snag

class ByteBufferReader(val refiller: ByteBufferRefiller) {

  def underflowExceptionToSnagLeft[T](ex: BufferUnderflowException, fieldName: String): Left[Snag, T] =
    Left(Snag(s"Could not read field $fieldName", Snag(ex)))

  def readField[T](name: String, nBytesNeeded: Int)(read: ByteBuffer => T): Either[Snag, T] = {
    try {
      refiller.read(nBytesNeeded)(read)
    } catch {
      case ex: BufferUnderflowException => underflowExceptionToSnagLeft(ex, name)
    }
  }

  def readFieldAssert[T](name: String, expected: T, nBytesNeeded: Int)(read: ByteBuffer => T): Either[Snag, T] = {
    for {
      value <- refiller.read(nBytesNeeded)(read)
      valueAgain <-
        if (value == expected) {
          Right(value)
        } else {
          Left(Snag(s"Field $name is $value, but should be $expected."))
        }
    } yield valueAgain
  }

  def readUnsignedByteField(name: String): Either[Snag, UnsignedByte] =
    readField(name, 1)(buffer => UnsignedByte(buffer.get()))

  def readUnsignedByteFieldAssert(name: String, expected: UnsignedByte): Either[Snag, UnsignedByte] =
    readFieldAssert(name, expected, 1)(buffer => UnsignedByte(buffer.get()))

  def readUnsignedShortField(name: String): Either[Snag, UnsignedShort] =
    readField(name, 2)(buffer => UnsignedShort(buffer.getShort()))

  def readUnsignedShortFieldAssert(name: String, expected: UnsignedShort): Either[Snag, UnsignedShort] =
    readFieldAssert(name, expected, 2)(buffer => UnsignedShort(buffer.getShort()))

  def readUnsignedIntField(name: String): Either[Snag, UnsignedInt] =
    readField(name, 4)(buffer => UnsignedInt(buffer.getInt()))

  def readUnsignedIntFieldAssert(name: String, expected: UnsignedInt): Either[Snag, UnsignedInt] =
    readFieldAssert(name, expected, 4)(buffer => UnsignedInt(buffer.getInt()))

  def unzip(unzippedSize: Int): Either[Snag, Array[Byte]] = {
    refiller.read(unzippedSize) { buffer =>
      val is = new ByteArrayInputStream(buffer.array, buffer.position(), unzippedSize)
      val unzippedBytes = new GZIPInputStream(is).readAllBytes()
      unzippedBytes
    }
  }
}
