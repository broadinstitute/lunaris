package lunaris.io.tbi

import lunaris.genomics.Region
import lunaris.io.ByteBufferReader
import lunaris.io.tbi.TBIFileReader.TBIChunksForSequenceEitherator.OKState
import lunaris.utils.{EitherSeqUtils, Eitherator}
import org.broadinstitute.yootilz.core.snag.Snag

import scala.collection.mutable

object TBIFileReader {

  private def readBinningIndex(reader: ByteBufferReader,
                               regions: Seq[Region]): Either[Snag, Map[Region, Seq[TBIChunk]]] = {
    var chunksByRegion: Map[Region, mutable.Builder[TBIChunk, Seq[TBIChunk]]] = Map.empty
    val snagOrUnit = for {
      nBins <- reader.readIntField("n_bin")
      _ <- EitherSeqUtils.repeat(nBins) {
        val snagOrRegionsAndChunks = for {
          bin <- reader.readUnsignedIntField("bin")
          nChunks <- reader.readIntField("n_chunk")
          binAsRegion = TBIBins.binAsRegion(bin.int)
          overlappingRegions = regions.filter(_.overlaps(binAsRegion))
          chunks <- {
            if (overlappingRegions.nonEmpty) {
              val snagOrChunks = EitherSeqUtils.fill(nChunks) {
                val snagOrChunk = for {
                  chunkBegin <- reader.readLongField("cnk_beg").map(TBIVirtualFileOffset(_))
                  chunkEnd <- reader.readLongField("cnk_beg").map(TBIVirtualFileOffset(_))
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
                              chunksByRegion: Map[Region, Seq[TBIChunk]]): Either[Snag, Map[Region, Seq[TBIChunk]]] = {
    var trimmedChunksByRegion: Map[Region, Seq[TBIChunk]] = chunksByRegion
    for {
      nIntervals <- reader.readIntField("n_intv")
      _ <- EitherSeqUtils.traverse(0 until nIntervals) { iInterval =>
        for {
          offset <- reader.readLongField("ioff").map(TBIVirtualFileOffset(_))
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

  private def readSequenceIndex(reader: ByteBufferReader, name: String,
                                regions: Seq[Region]): Either[Snag, Map[Region, Seq[TBIChunk]]] = {
    for {
      chunksByRegion <- readBinningIndex(reader, regions)
      trimmedChunksByRegion <- readLinearIndex(reader, chunksByRegion)
    } yield trimmedChunksByRegion
  }

  def readChunksForSequence(reader: ByteBufferReader, regionsBySequence: Map[String, Seq[Region]]): TBIChunksForSequenceEitherator =
    TBIChunksForSequenceEitherator(reader, regionsBySequence)

  case class TBIChunksForSequence(name: String, chunksByRegion: Map[Region, Seq[TBIChunk]])

  class TBIChunksForSequenceEitherator(reader: ByteBufferReader,
                                       regionsBySequence: Map[String, Seq[Region]])
    extends Eitherator[TBIChunksForSequence] {
    var snagOrState: Either[Snag, OKState] = TBIFileHeader.read(reader).map(OKState(_))

    override def next(): Either[Snag, Option[TBIChunksForSequence]] = {
      snagOrState match {
        case Left(snag) => Left(snag)
        case Right(state) =>
          if (state.namesIter.hasNext) {
            val name = state.namesIter.next()
            val regions = regionsBySequence.getOrElse(name, Seq.empty)
            TBIFileReader.readSequenceIndex(reader, name, regions) match {
              case Left(snag) =>
                snagOrState = Left(snag)
                Left(snag)
              case Right(chunksByRegion) =>
                Right(Some(TBIChunksForSequence(name, chunksByRegion)))
            }
          } else {
            Right(None)
          }
      }
    }
  }

  object TBIChunksForSequenceEitherator {

    def apply(reader: ByteBufferReader, regionsBySequence: Map[String, Seq[Region]]): TBIChunksForSequenceEitherator =
      new TBIChunksForSequenceEitherator(reader, regionsBySequence)

    class OKState(val header: TBIFileHeader, val namesIter: Iterator[String])

    object OKState {
      def apply(header: TBIFileHeader): OKState = {
        val namesIter = header.names.iterator
        new OKState(header, namesIter)
      }
    }

  }

  case class TBIChunkWithSequenceAndRegion(chunk: TBIChunk, name: String, regions: Set[Region])

  def readChunksWithSequenceAndRegions(reader: ByteBufferReader,
                                       regionsBySequence: Map[String, Seq[Region]]):
  Eitherator[TBIChunkWithSequenceAndRegion] = {
    readChunksForSequence(reader, regionsBySequence).flatMap { chunksForSequence =>
      ???  //  TODO
    }
  }



}
