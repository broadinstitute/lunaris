package lunaris.io

import java.nio.ByteBuffer
import java.nio.channels.ReadableByteChannel

import lunaris.io.bgz.BGZBlock
import lunaris.io.bgz.BGZBlock.BGZBlockWithPos
import lunaris.io.tbi.TBIChunk
import lunaris.utils.{ByteBox, DebugUtils, ReadableByteChannelUtils}
import org.broadinstitute.yootilz.core.snag.Snag

import scala.util.control.NonFatal

trait ByteBufferRefiller {
  def byteBox: ByteBox

  protected def writeToBuffer(buffer: ByteBuffer, nBytesNeeded: Int): Either[Snag, Int]

  def isExhausted: Boolean

  final def refill(nBytesNeeded: Int): Either[Snag, Int] = {
    byteBox.writeToBuffer { buffer =>
      for {
        nBytesWritten <- writeToBuffer(buffer, nBytesNeeded)
        _ <- {
          val nBytesRemaining = buffer.position()
          if (nBytesRemaining < nBytesNeeded) {
            Left(Snag(
              s"Even after trying to refill buffer, only have $nBytesRemaining bytes remaining, but need $nBytesNeeded"
            ))
          } else {
            Right(())
          }
        }
      } yield nBytesWritten
    }
  }

  def makeAvailable(nBytesNeeded: Int): Either[Snag, Int] = {
    if (byteBox.remaining < nBytesNeeded) {
      refill(nBytesNeeded)
    } else {
      Right(byteBox.remaining)
    }
  }

  def read[T](reader: ByteBuffer => T): Either[Snag, T] = {
    try {
      Right(byteBox.readFromBuffer(reader))
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
      _ = byteBox.skip(nBytesNeeded)
    } yield ()
  }
}

object ByteBufferRefiller {

  trait Seekable extends ByteBufferRefiller {
    def skipTo(pos: Long): Unit
  }

  def apply(channel: ReadableByteChannel, bufferSize: Int): FromChannel = new FromChannel(channel, bufferSize)

  class FromChannel(val channel: ReadableByteChannel, val bufferSize: Int) extends ByteBufferRefiller.Seekable {
    override val byteBox: ByteBox = ByteBox(bufferSize)

    private var nBytesWrittenOpt: Option[Int] = None

    protected def writeToBuffer(buffer: ByteBuffer, nBytesNeeded: Int): Either[Snag, Int] = {
      try {
        val nBytesWritten = channel.read(buffer)
        nBytesWrittenOpt = Some(nBytesWritten)
        Right(nBytesWritten)
      } catch {
        case NonFatal(ex) => Left(Snag("Exception while trying to refill buffer", Snag(ex)))
      }
    }

    override def skipTo(pos: Long): Unit = {
      byteBox.clear()
      ReadableByteChannelUtils.seek(channel, pos)
    }

    override def isExhausted: Boolean = nBytesWrittenOpt.contains(0) && byteBox.remaining == 0
  }

  def bgunzip(rawReadChannel: ReadableByteChannel, bufferSize: Int): BGUnzipByteBufferRefiller =
    BGUnzipByteBufferRefiller(rawReadChannel, bufferSize)

  class BGUnzipByteBufferRefiller(rawReadChannel: ReadableByteChannel,
                                  val bufferSize: Int)
    extends ByteBufferRefiller {
    private var _currentChunk: TBIChunk = TBIChunk.wholeFile
    val bgzBlockEitherator: BGZBlock.BlockEitherator = BGZBlock.newBlockEitherator(rawReadChannel)
    override val byteBox: ByteBox = ByteBox(bufferSize)
    var currentBytesOpt: Option[CurrentBytes] = None
    var usedLastBlock: Boolean = false

    override def isExhausted: Boolean = usedLastBlock && currentBytesOpt.isEmpty && byteBox.remaining == 0

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

    def clearUnzippedData(): Unit = {
      currentBytesOpt = None
      byteBox.clear()
    }

    def currentChunk: TBIChunk = _currentChunk

    def currentChunk_=(chunk: TBIChunk): Unit = {
      _currentChunk = chunk
      val blockStartNew = chunk.begin.offsetOfBlock
      bgzBlockEitherator.skipToOffset(blockStartNew)
      clearUnzippedData()
      usedLastBlock = false
    }

    protected def writeToBuffer(buffer: ByteBuffer, nBytesNeeded: Int): Either[Snag, Int] = {
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
            if (bgzBlockEitherator.readPos <= _currentChunk.end.offsetOfBlock) {
              bgzBlockEitherator.next() match {
                case Left(snag) => snagOpt = Some(snag)
                case Right(None) => snagOpt =
                  Some(Snag(s"Need $nBytesNeeded bytes, but only ${buffer.position()} available."))
                case Right(Some(BGZBlockWithPos(block, pos))) =>
                  val unzippedBytes = block.unzippedData.bytes
                  val blockIsFirstInChunk = pos == _currentChunk.begin.offsetOfBlock
                  val blockIsLastInChunk = pos == _currentChunk.end.offsetOfBlock
                  usedLastBlock = blockIsLastInChunk
                  val bytesForRefill =
                    (blockIsFirstInChunk, blockIsLastInChunk) match {
                      case (false, false) =>
                        unzippedBytes
                      case (false, true) =>
                        val nBytesForRefill = _currentChunk.end.offsetInBlock
                        Array.copyOf(unzippedBytes, nBytesForRefill)
                      case (true, _) =>
                        val offset = _currentChunk.begin.offsetInBlock
                        val endPos =
                          if(blockIsLastInChunk) {
                            _currentChunk.end.offsetInBlock
                          } else {
                            unzippedBytes.length
                          }
                        val nBytesForRefill = endPos - offset
                        val byteForRefillNew = new Array[Byte](nBytesForRefill)
                        Array.copy(unzippedBytes, offset, byteForRefillNew, 0, nBytesForRefill)
                        byteForRefillNew
                    }
                  currentBytesOpt = Some(new CurrentBytes(bytesForRefill, 0))
              }
            } else {
              snagOpt = Some(Snag(s"Need $nBytesNeeded bytes, but only ${buffer.position()} available."))
            }
          }
        }
        snagOpt match {
          case Some(snag) => Left(snag)
          case None => Right(nBytesRead)
        }
      }
    }
  }

  object BGUnzipByteBufferRefiller {
    def apply(rawReadChannel: ReadableByteChannel, bufferSize: Int): BGUnzipByteBufferRefiller =
      new BGUnzipByteBufferRefiller(rawReadChannel, bufferSize)
  }

}
