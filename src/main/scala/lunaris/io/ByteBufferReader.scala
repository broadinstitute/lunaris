package lunaris.io

import java.io.ByteArrayInputStream
import java.nio.{BufferUnderflowException, ByteBuffer}
import java.util.zip.GZIPInputStream

import lunaris.io.IntegersIO.{UnsignedByte, UnsignedInt, UnsignedShort}
import org.broadinstitute.yootilz.core.snag.Snag

class ByteBufferReader(val refiller: ByteBufferRefiller) {

  def skip(nBytesNeeded: Int): Either[Snag, Unit] = {
    try {
      refiller.skip(nBytesNeeded)
    } catch {
      case ex: BufferUnderflowException => Left(Snag(s"Could not skip $nBytesNeeded bytes.", Snag(ex)))
    }
  }

  def readField[T](name: String, nBytesNeeded: Int)(read: ByteBuffer => T): Either[Snag, T] = {
    try {
      refiller.read(nBytesNeeded)(read)
    } catch {
      case ex: BufferUnderflowException => Left(Snag(s"Could not read field $name", Snag(ex)))
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

  def readByteField(name: String): Either[Snag, Byte] =
    readField(name, 1)(_.get())

  def readByteFieldAssert(name: String, expected: Byte): Either[Snag, Byte] =
    readFieldAssert(name, expected, 1)(_.get())

  def readUnsignedByteField(name: String): Either[Snag, UnsignedByte] =
    readField(name, 1)(buffer => UnsignedByte(buffer.get()))

  def readUnsignedByteFieldAssert(name: String, expected: UnsignedByte): Either[Snag, UnsignedByte] =
    readFieldAssert(name, expected, 1)(buffer => UnsignedByte(buffer.get()))

  def readUnsignedShortField(name: String): Either[Snag, UnsignedShort] =
    readField(name, 2)(buffer => UnsignedShort(buffer.getShort()))

  def readUnsignedShortFieldAssert(name: String, expected: UnsignedShort): Either[Snag, UnsignedShort] =
    readFieldAssert(name, expected, 2)(buffer => UnsignedShort(buffer.getShort()))

  def readIntField(name: String): Either[Snag, Int] =
    readField(name, 4)(_.getInt())

  def readIntFieldAssert(name: String, expected: Int): Either[Snag, Int] =
    readFieldAssert(name, expected, 4)(_.getInt())

  def readUnsignedIntField(name: String): Either[Snag, UnsignedInt] =
    readField(name, 4)(buffer => UnsignedInt(buffer.getInt()))

  def readUnsignedIntFieldAssert(name: String, expected: UnsignedInt): Either[Snag, UnsignedInt] =
    readFieldAssert(name, expected, 4)(buffer => UnsignedInt(buffer.getInt()))

  def readLongField(name: String): Either[Snag, Long] =
    readField(name, 8)(_.getLong())

  def unzip(blockSize: Int): Either[Snag, Array[Byte]] = {
    refiller.read(blockSize) { buffer =>
      val is = new ByteArrayInputStream(buffer.array, buffer.position(), blockSize)
      val unzippedBytes = new GZIPInputStream(is).readAllBytes()
      buffer.position(buffer.position() + blockSize)
      unzippedBytes
    }
  }

  def readBytes(nBytes: Int): Either[Snag, Array[Byte]] = readField("bytes", nBytes){ buffer =>
    val bytes = new Array[Byte](nBytes)
    buffer.get(bytes)
    bytes
  }

  def readLine(): Either[Snag, String] = {
    val arrayBuilder = Array.newBuilder[Byte]
    var snagOpt: Option[Snag] = None
    var eol: Boolean = false
    while(!eol && snagOpt.isEmpty) {
      readByteField("line") match {
        case Left(snag) => snagOpt = Some(snag)
        case Right(byte) =>
          if(byte == '\n'.toByte) {
            eol = true
          } else {
            arrayBuilder += byte
          }
      }
    }
    snagOpt match {
      case Some(snag) => Left(snag)
      case None => Right(new String(arrayBuilder.result()))
    }
  }
}

object ByteBufferReader {
  def apply(refiller: ByteBufferRefiller): ByteBufferReader = new ByteBufferReader(refiller)
}
