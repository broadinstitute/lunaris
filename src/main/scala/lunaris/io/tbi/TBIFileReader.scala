package lunaris.io.tbi

import lunaris.genomics.Region
import lunaris.io.ByteBufferReader
import lunaris.io.IntegersIO.UnsignedInt
import org.broadinstitute.yootilz.core.snag.Snag

import scala.collection.mutable

object TBIFileReader {

  trait TBIConsumer {
    def consumeHeader(header: TBIFileHeader): Unit

    def startSequenceIndex(name: String): Unit

    def regionsOverlappingBin(bin: UnsignedInt): Set[Region]

    def consumeChunks(chunks: Seq[Chunk]): Unit

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

  def traverse[A, B](xs: Seq[A])(f: A => Either[Snag, B]): Either[Snag, Seq[B]] = {
    val builder = Seq.newBuilder[B]
    val iter = xs.iterator
    while (iter.hasNext) {
      f(iter.next) match {
        case Right(b) => builder += b
        case Left(snag) => return Left(snag)
      }
    }
    Right(builder.result)
  }

  def fill[A](n: Int)(f: => Either[Snag, A]): Either[Snag, Seq[A]] = {
    val builder = Seq.newBuilder[A]
    var i: Int = 0
    while (i < n) {
      f match {
        case Right(b) => builder += b
        case Left(snag) => return Left[Snag, Seq[A]](snag)
      }
      i += 1
    }
    Right(builder.result)
  }



  case class Chunk(begin: TbiVirtualFileOffset, end: TbiVirtualFileOffset)

  private def readBinningIndex(reader: ByteBufferReader,
                               consumer: TBIConsumer): Either[Snag, Map[Region, Seq[Chunk]]] = {
    var chunksByRegion: Map[Region, mutable.Builder[Chunk, Seq[Chunk]]] = Map.empty
    val snagOrUnit = for {
      nBins <- reader.readIntField("n_bin")
      _ <- repeat(nBins) {
        val snagOrRegionsAndChunks = for {
          bin <- reader.readUnsignedIntField("bin")
          nChunks <- reader.readIntField("n_chunk")
          regions = consumer.regionsOverlappingBin(bin)
          chunks <- {
            if (regions.nonEmpty) {
              val snagOrChunks = fill(nChunks) {
                val snagOrChunk = for {
                  chunkBegin <- reader.readLongField("cnk_beg").map(TbiVirtualFileOffset(_))
                  chunkEnd <- reader.readLongField("cnk_beg").map(TbiVirtualFileOffset(_))
                } yield Chunk(chunkBegin, chunkEnd)
                snagOrChunk
              }
              snagOrChunks.fold(consumer.consumeSnag, consumer.consumeChunks)
              snagOrChunks
            } else {
              val bytesPerChunk = 16
              reader.skip(nChunks*bytesPerChunk).map(_ => Seq.empty[Chunk])
            }
          }
        } yield (regions, chunks)
        snagOrRegionsAndChunks.map {
          case (regions, chunks) =>
          regions.foreach { region =>
            val builder = chunksByRegion.getOrElse(region, Seq.newBuilder[Chunk])
            builder ++= chunks
          }
        }
      }
    } yield ()
    snagOrUnit.map { _ =>
      chunksByRegion.view.mapValues(_.result()).toMap
    }
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
