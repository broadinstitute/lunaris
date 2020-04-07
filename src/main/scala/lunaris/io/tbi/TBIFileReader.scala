package lunaris.io.tbi

import lunaris.io.ByteBufferReader
import lunaris.io.IntegersIO.UnsignedInt
import org.broadinstitute.yootilz.core.snag.Snag
import scala.collection.Factory

object TBIFileReader {

  trait TBIConsumer {
    def consumeHeader(header: TBIFileHeader): Unit

    def startSequenceIndex(name: String): Unit

    def consumeNBins(nBins: Int): Unit

    def consumeBinNumber(bin: UnsignedInt): Unit

    def consumeNChunks(nChunks: Int): Unit

    def consumeChunk(chunk: Chunk): Unit

    def consumeNIntervals(nIntervals: Int): Unit

    def consumeIntervalOffset(offset: TbiVirtualFileOffset): Unit

    def doneWithSequenceIndex(name: String): Unit

    def consumeSnag(snag: Snag): Unit
  }

  def consume[T](either: Either[Snag, T])(valueFun: T => Unit)(snagFun: Snag => Unit): Either[Snag, T] = {
    either match {
      case Left(snag) => snagFun(snag)
      case Right(value) => valueFun(value)
    }
    either
  }

  def foreach[A, B](xs: Seq[A])(f: A => Either[Snag, B]): Either[Snag, Unit] = {
    val i = xs.iterator
    while (i.hasNext) f(i.next) match {
      case Right(_) => ()
      case Left(e) => return Left(e)
    }
    Right(())
  }

  def repeat[A](n: Int)(fun: => Either[Snag, A]): Either[Snag, Unit] = {
    var snagOpt: Option[Snag] = None
    var i: Int = 0
    while (snagOpt.isEmpty && i < n) {
      fun match {
        case Left(snag) => snagOpt = Some(snag)
        case Right(_) => ()
      }
      i += 1
    }
    snagOpt match {
      case Some(snag) => Left(snag)
      case None => Right(())
    }
  }

  case class Chunk(begin: TbiVirtualFileOffset, end: TbiVirtualFileOffset)

  private def readBinningIndex(reader: ByteBufferReader, consumer: TBIConsumer): Either[Snag, Unit] = {
    for {
      nBins <- consume(reader.readIntField("n_bin"))(consumer.consumeNBins)(consumer.consumeSnag)
      _ <- repeat(nBins) {
        for {
          _ <- consume(reader.readUnsignedIntField("bin"))(consumer.consumeBinNumber)(consumer.consumeSnag)
          nChunks <- consume(reader.readIntField("n_chunk"))(consumer.consumeNChunks)(consumer.consumeSnag)
          _ <- repeat(nChunks) {
            val snagOrChunk = for {
              chunkBegin <- reader.readLongField("cnk_beg").map(TbiVirtualFileOffset(_))
              chunkEnd <- reader.readLongField("cnk_beg").map(TbiVirtualFileOffset(_))
            } yield Chunk(chunkBegin, chunkEnd)
            snagOrChunk.fold(consumer.consumeSnag, consumer.consumeChunk)
            snagOrChunk
          }
        } yield ()
      }
    } yield ()
  }

  private def readLinearIndex(reader: ByteBufferReader, consumer: TBIConsumer): Either[Snag, Unit] = {
    for {
      nIntervals <- consume(reader.readIntField("n_intv"))(consumer.consumeNIntervals)(consumer.consumeSnag)
      _ <- repeat(nIntervals) {
        consume {
          reader.readLongField("ioff").map(TbiVirtualFileOffset(_))
        }(consumer.consumeIntervalOffset)(consumer.consumeSnag)
      }
    } yield ()
  }

  private def readSequenceIndex(reader: ByteBufferReader, name: String, consumer: TBIConsumer): Either[Snag, Unit] = {
    consumer.startSequenceIndex(name)
    for {
      _ <- readBinningIndex(reader, consumer)
      _ <- readLinearIndex(reader, consumer)
      _ = consumer.doneWithSequenceIndex(name)
    } yield ()
  }

  def readFile(reader: ByteBufferReader, consumer: TBIConsumer): Unit = {
    for {
      header <- consume(TBIFileHeader.read(reader))(consumer.consumeHeader)(consumer.consumeSnag)
      _ <- foreach(header.names) { name =>
        readSequenceIndex(reader, name, consumer)
      }
    } yield ()
  }
}
