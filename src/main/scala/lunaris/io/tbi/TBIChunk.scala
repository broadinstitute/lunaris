package lunaris.io.tbi

import lunaris.genomics.Region
import lunaris.io.tbi.TBIFileReader.{TBIChunkWithSequenceAndRegions, TBIChunksForSequence}
import lunaris.utils.IteratorUtils

case class TBIChunk(begin: TBIVirtualFileOffset, end: TBIVirtualFileOffset) extends Ordered[TBIChunk] {
  override def compare(that: TBIChunk): Int = {
    val compareBegin = begin.compare(that.begin)
    if (compareBegin != 0) {
      compareBegin
    } else {
      end.compare(that.end)
    }
  }
}

object TBIChunk {
  val wholeFile: TBIChunk = TBIChunk(TBIVirtualFileOffset.startOfFile, TBIVirtualFileOffset.endOfFile)

  def consolidateTwoChunks(chunk1: TBIChunk, chunk2: TBIChunk): Seq[TBIChunk] = {
    if (chunk1.end < chunk2.begin) {
      Seq(chunk1, chunk2)
    } else if (chunk2.end < chunk1.begin) {
      Seq(chunk2, chunk1)
    } else {
      val begin = if (chunk1.begin < chunk2.begin) chunk1.begin else chunk2.begin
      val end = if (chunk1.end > chunk2.end) chunk1.end else chunk2.end
      Seq(TBIChunk(begin, end))
    }
  }

  def consolidateChunks(chunks: Seq[TBIChunk]): Seq[TBIChunk] = {
    if (chunks.isEmpty) {
      Seq.empty
    } else {
      val builder = Seq.newBuilder[TBIChunk]
      var currentChunk = chunks.head
      var chunksRemaining = chunks.tail
      while (chunksRemaining.nonEmpty) {
        val nextChunk = chunksRemaining.head
        chunksRemaining = chunksRemaining.tail
        consolidateTwoChunks(currentChunk, nextChunk) match {
          case Seq(consolidatedChunk) =>
            currentChunk = consolidatedChunk
          case Seq(chunk1, chunk2) =>
            builder += chunk1
            currentChunk = chunk2
        }
      }
      builder += currentChunk
      builder.result()
    }
  }

  def consolidateSeqsOfChunks(seqsOfChunks: Iterable[Seq[TBIChunk]]): Seq[TBIChunk] = {
    var iterators = seqsOfChunks.map(IteratorUtils.newBufferedIterator).filter(_.hasNext)
    val builder = Seq.newBuilder[TBIChunk]
    while (iterators.nonEmpty) {
      var nextChunk = {
        val iterIter = iterators.iterator
        var iterOfNext = iterIter.next()
        while (iterIter.hasNext) {
          val nextIter = iterIter.next()
          if (nextIter.head < iterOfNext.head) {
            iterOfNext = nextIter
          }
        }
        iterOfNext.next()
      }
      var consolidating: Boolean = true
      while (consolidating) {
        consolidating = false
        iterators = iterators.filter(_.hasNext)
        for (iterator <- iterators) {
          val head = iterator.head
          if (head.begin <= nextChunk.end) {
            consolidating = true
            iterator.next()
            if (head.end > nextChunk.end) {
              nextChunk = TBIChunk(nextChunk.begin, head.end)
            }
          }
        }
      }
      builder += nextChunk
      iterators = iterators.filter(_.hasNext)
    }
    builder.result()
  }

  case class TBIChunkWithRegions(chunk: TBIChunk, regions: Set[Region])

  def consolidateChunksByRegion(chunksByRegion: Map[Region, Seq[TBIChunk]]): Seq[TBIChunkWithRegions] = {
    var iterators = chunksByRegion.collect {
      case (region, chunks) => (region, IteratorUtils.newBufferedIterator(chunks))
    }.filter(_._2.hasNext)
    val builder = Seq.newBuilder[TBIChunkWithRegions]
    while (iterators.nonEmpty) {
      var nextChunkWithRegion = {
        val iterIter = iterators.iterator
        var iterOfNext = iterIter.next()
        while (iterIter.hasNext) {
          val nextIter = iterIter.next()
          if (nextIter._2.head < iterOfNext._2.head) {
            iterOfNext = nextIter
          }
        }
        TBIChunkWithRegions(iterOfNext._2.next(), Set(iterOfNext._1))
      }
      var consolidating: Boolean = true
      while (consolidating) {
        consolidating = false
        iterators = iterators.filter(_._2.hasNext)
        for (iterator <- iterators) {
          val region = iterator._1
          val chunkIter = iterator._2
          val head = chunkIter.head
          if (head.begin <= nextChunkWithRegion.chunk.end) {
            consolidating = true
            chunkIter.next()
            nextChunkWithRegion = nextChunkWithRegion.copy(regions = nextChunkWithRegion.regions + region)
            if (head.end > nextChunkWithRegion.chunk.end) {
              nextChunkWithRegion =
                TBIChunkWithRegions(TBIChunk(nextChunkWithRegion.chunk.begin, head.end),
                  nextChunkWithRegion.regions + region)
            }
          }
        }
      }
      builder += nextChunkWithRegion
      iterators = iterators.filter(_._2.hasNext)
    }
    builder.result()
  }
}
