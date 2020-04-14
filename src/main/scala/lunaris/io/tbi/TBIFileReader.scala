package lunaris.io.tbi

import lunaris.genomics.Region
import lunaris.io.ByteBufferReader
import lunaris.io.IntegersIO.UnsignedInt
import lunaris.utils.EitherSeqUtils
import org.broadinstitute.yootilz.core.snag.Snag

import scala.collection.mutable

object TBIFileReader {

  trait TBIConsumer {
    def startSequenceIndex(name: String): Unit

    def regionsOverlappingBin(bin: UnsignedInt): Set[Region]

    def consumeChunksForSequence(name: String, chunksByRegion: Map[Region, Seq[TBIChunk]]): Unit

    def consumeSnag(snag: Snag): Unit
  }

  private def readBinningIndex(reader: ByteBufferReader,
                               consumer: TBIConsumer): Either[Snag, Map[Region, Seq[TBIChunk]]] = {
    var chunksByRegion: Map[Region, mutable.Builder[TBIChunk, Seq[TBIChunk]]] = Map.empty
    val snagOrUnit = for {
      nBins <- reader.readIntField("n_bin")
      _ <- EitherSeqUtils.repeat(nBins) {
        val snagOrRegionsAndChunks = for {
          bin <- reader.readUnsignedIntField("bin")
          nChunks <- reader.readIntField("n_chunk")
          regions = consumer.regionsOverlappingBin(bin)
          chunks <- {
            if (regions.nonEmpty) {
              val snagOrChunks = EitherSeqUtils.fill(nChunks) {
                val snagOrChunk = for {
                  chunkBegin <- reader.readLongField("cnk_beg").map(TbiVirtualFileOffset(_))
                  chunkEnd <- reader.readLongField("cnk_beg").map(TbiVirtualFileOffset(_))
                } yield TBIChunk(chunkBegin, chunkEnd)
                snagOrChunk
              }
              snagOrChunks
            } else {
              val bytesPerChunk = 16
              reader.skip(nChunks * bytesPerChunk).map(_ => Seq.empty[TBIChunk])
            }
          }
        } yield (regions, chunks)
        snagOrRegionsAndChunks.map {
          case (regions, chunks) =>
            regions.foreach { region =>
              val builder = chunksByRegion.get(region) match {
                case Some(builderForRegion) => builderForRegion
                case None =>
                  val builderForRegion = Seq.newBuilder[TBIChunk]
                  chunksByRegion += (region -> builderForRegion)
                  builderForRegion
              }
              builder ++= chunks
            }
        }
      }
    } yield ()
    snagOrUnit.map { _ =>
      chunksByRegion.view.mapValues(_.result().sorted).toMap
    }
  }

  private def readLinearIndex(reader: ByteBufferReader,
                              chunksByRegion: Map[Region, Seq[TBIChunk]],
                              consumer: TBIConsumer): Either[Snag, Map[Region, Seq[TBIChunk]]] = {
    var trimmedChunksByRegion: Map[Region, Seq[TBIChunk]] = chunksByRegion
    for {
      nIntervals <- reader.readIntField("n_intv")
      _ <- EitherSeqUtils.traverse(0 until nIntervals) { iInterval =>
        for {
          offset <- reader.readLongField("ioff").map(TbiVirtualFileOffset(_))
          _ = for ((region, chunks) <- trimmedChunksByRegion) {
            if (TBIIntervals.firstOverlappingIntervalFor(region) == iInterval) {
              val trimmedChunks = TBIIntervals.trimChunks(chunks, offset)
              trimmedChunksByRegion += (region -> trimmedChunks)
            }
          }
        } yield ()
      }
    } yield trimmedChunksByRegion
  }

  private def readSequenceIndex(reader: ByteBufferReader, name: String, consumer: TBIConsumer): Either[Snag, Unit] = {
    consumer.startSequenceIndex(name)
    for {
      chunksByRegion <- readBinningIndex(reader, consumer)
      trimmedChunksByRegion <- readLinearIndex(reader, chunksByRegion, consumer)
      _ = consumer.consumeChunksForSequence(name, trimmedChunksByRegion)
    } yield ()
  }

  def readFile(reader: ByteBufferReader, consumer: TBIConsumer): Unit = {
    for {
      header <- TBIFileHeader.read(reader)
      _ <- EitherSeqUtils.foreach(header.names) { name =>
        readSequenceIndex(reader, name, consumer)
      }
    } yield ()
  }
}
