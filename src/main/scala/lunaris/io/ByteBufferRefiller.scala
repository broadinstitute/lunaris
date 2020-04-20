package lunaris.io

import java.nio.ByteBuffer
import java.nio.channels.ReadableByteChannel

import lunaris.io.bgz.BGZBlock
import lunaris.io.tbi.TBIChunk
import lunaris.utils.ReadableByteChannelUtils
import org.broadinstitute.yootilz.core.snag.Snag

import scala.util.control.NonFatal

trait ByteBufferRefiller {
  def buffer: ByteBuffer

  def refill(nBytesNeeded: Int): Either[Snag, Int]

  def makeAvailable(nBytesNeeded: Int): Either[Snag, Int] = {
    if (buffer.remaining() < nBytesNeeded) {
      refill(nBytesNeeded)
    } else {
      Right(buffer.remaining())
    }
  }

  def read[T](reader: ByteBuffer => T): Either[Snag, T] = {
    try {
      Right(reader(buffer))
    } catch {
      case NonFatal(ex) => Left(Snag("Could not read from buffer", Snag(ex)))
    }
  }

  def read[T](nBytesNeeded: Int)(reader: ByteBuffer => T): Either[Snag, T] = {
    for {
      _ <- makeAvailable(nBytesNeeded)
      value <- read(reader)
    } yield value
  }

  def skip[T](nBytesNeeded: Int): Either[Snag, Unit] = {
    for {
      _ <- makeAvailable(nBytesNeeded)
      _ = buffer.position(buffer.position() + nBytesNeeded)
    } yield ()
  }
}

object ByteBufferRefiller {

  trait Seekable extends ByteBufferRefiller {
    def skipTo(pos: Long): Unit
  }

  def apply(channel: ReadableByteChannel, bufferSize: Int): FromChannel = new FromChannel(channel, bufferSize)

  class FromChannel(val channel: ReadableByteChannel, val bufferSize: Int) extends ByteBufferRefiller.Seekable {
    val bytes: Array[Byte] = new Array[Byte](bufferSize)
    override val buffer: ByteBuffer = ByteBuffer.wrap(bytes)
    channel.read(buffer)
    buffer.flip()

    override def refill(nBytesNeeded: Int): Either[Snag, Int] = {
      try {
        buffer.compact()
        val nBytesRead = channel.read(buffer)
        buffer.flip()
        val nBytesRemaining = buffer.remaining()
        if (buffer.remaining() < nBytesNeeded) {
          Left(Snag(
            s"Even after trying to refill buffer, only have $nBytesRemaining bytes remaining, but need $nBytesNeeded"
          ))
        } else {
          Right(nBytesRead)
        }
      } catch {
        case NonFatal(ex) => Left(Snag("Exception while trying to refill buffer", Snag(ex)))
      }
    }

    override def skipTo(pos: Long): Unit = {
      ReadableByteChannelUtils.seek(channel, pos)
    }
  }

  def bgunzip(rawReadChannel: ReadableByteChannel, bufferSize: Int): BGUnzipByteBufferRefiller =
    BGUnzipByteBufferRefiller(rawReadChannel, bufferSize)

  class BGUnzipByteBufferRefiller(rawReadChannel: ReadableByteChannel, val bufferSize: Int)
    extends ByteBufferRefiller {
    var currentChunk: TBIChunk = TBIChunk.wholeFile
    val bgzBlockEitherator: BGZBlock.BlockEitherator = BGZBlock.newBlockEitherator(rawReadChannel)
    override val buffer: ByteBuffer = ByteBuffer.allocate(bufferSize)
    var currentBytesOpt: Option[CurrentBytes] = None
    //writeToBuffer(1)
    buffer.flip()

    class CurrentBytes(val bytes: Array[Byte], val nAlreadyRead: Int) {
      def nBytesAvailable: Int = bytes.length - nAlreadyRead

      def afterReadingOpt(nBytesRead: Int): Option[CurrentBytes] = {
        val nAlreadyReadNew = nAlreadyRead + nBytesRead
        if (nAlreadyReadNew < bytes.length) {
          Some(new CurrentBytes(bytes, nAlreadyReadNew))
        } else {
          None
        }
      }
    }

    private def debug(buffer: ByteBuffer): Unit = {
      println(s"Buffer position ${buffer.position()}, limit ${buffer.limit()}, capacity ${buffer.capacity()}")
    }

    private def consumeNextBlock(bgzBlock: BGZBlock): Unit = {
      val bytes = bgzBlock.unzippedData.bytes
      currentBytesOpt = Some(new CurrentBytes(bytes, 0))
    }

    private def writeToBuffer(nBytesNeeded: Int): Either[Snag, Int] = {
      if (buffer.position() >= nBytesNeeded) {
        Right(0)
      } else if (nBytesNeeded > buffer.capacity()) {
        Left(Snag(s"Need to read $nBytesNeeded, but buffer capacity is only ${buffer.capacity()}."))
      } else {
        var nBytesRead: Int = 0
        var snagOpt: Option[Snag] = None
        while (snagOpt.isEmpty && buffer.position() < nBytesNeeded) {
          currentBytesOpt match {
            case Some(currentBytes) =>
              val nBytesBufferSpace = buffer.capacity() - buffer.position()
              val nBytesAvailable = currentBytes.nBytesAvailable
              val nBytesToRead = if (nBytesAvailable > nBytesBufferSpace) nBytesBufferSpace else nBytesAvailable
              buffer.put(currentBytes.bytes, currentBytes.nAlreadyRead, nBytesToRead)
              nBytesRead += nBytesToRead
              currentBytesOpt = currentBytes.afterReadingOpt(nBytesRead)
            case None => ()
          }
          if (buffer.position() < nBytesNeeded) {
            bgzBlockEitherator.next() match {
              case Left(snag) => snagOpt = Some(snag)
              case Right(None) => snagOpt =
                Some(Snag(s"Need $nBytesNeeded bytes, but only ${buffer.position()} available."))
              case Right(Some(bgzBlock)) =>
                consumeNextBlock(bgzBlock)
            }
          }
        }
        snagOpt match {
          case Some(snag) => Left(snag)
          case None => Right(nBytesRead)
        }
      }
    }

    override def refill(nBytesNeeded: Int): Either[Snag, Int] = {
      buffer.compact()
      val snagOrBytesWritten = writeToBuffer(nBytesNeeded)
      buffer.flip()
      snagOrBytesWritten
    }
  }

  object BGUnzipByteBufferRefiller {
    def apply(rawReadChannel: ReadableByteChannel, bufferSize: Int): BGUnzipByteBufferRefiller =
      new BGUnzipByteBufferRefiller(rawReadChannel, bufferSize)
  }

}
