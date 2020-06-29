package lunaris.utils

import java.io.{FilterInputStream, IOException, InputStream}
import java.util

import lunaris.utils.ReplacerInputStream.PreReplaceBuffer.ReadResult
import lunaris.utils.ReplacerInputStream.{PostReplaceBuffer, PreReplaceBuffer, ReplacerMap}
import lunaris.utils.ReplacerInputStream.ReplacerMap.{MatchResult, MatchesBeginning, NoMatchForFirstNBytes}

class ReplacerInputStream(in: InputStream,
                          replacerMap: ReplacerMap,
                          minChunkSize: Int = ReplacerInputStream.chunkSizeDefault)
  extends FilterInputStream(in) {

  private val targetChunkSize: Int = Math.max(replacerMap.maxPatternSize, minChunkSize)
  private val preReplaceBuffer: PreReplaceBuffer = PreReplaceBuffer(targetChunkSize)
  private val postReplaceBuffer: PostReplaceBuffer = PostReplaceBuffer.newEmpty()
  private var haveReachedEndOfIn: Boolean = false

  override def read(): Int = {
    tryToMakeAvailable(1)
    postReplaceBuffer.popOneByte()
  }

  override def read(buff: Array[Byte]): Int = read(buff, 0, buff.length)

  override def read(buff: Array[Byte], off: Int, len: Int): Int = {
    tryToMakeAvailable(len)
    postReplaceBuffer.popBytes(buff, off, len)
  }

  override def skip(n: Long): Long = {
    var nSkipped: Long = 0
    var mightGetMore: Boolean = true
    while(nSkipped < n && mightGetMore) {
      val nChunk = Math.min(n - nSkipped, ReplacerInputStream.skipChunkSize).toInt
      tryToMakeAvailable(nChunk)
      val nSkippedNew = postReplaceBuffer.dropBytes(nChunk)
      if(nSkippedNew > 0) {
        nSkipped += nSkippedNew
      } else {
        mightGetMore = false
      }
    }
    nSkipped
  }

  override def available(): Int = {
    tryToMakeAvailable(targetChunkSize)
    postReplaceBuffer.size
  }

  override def mark(readlimit: Int): Unit = throw new IOException("ReplacerInputStream does not support mark().")

  override def reset(): Unit = throw new IOException("ReplacerInputStream does not support mark().")

  override def markSupported(): Boolean = false

  private def tryToMakeAvailable(nBytesRequested: Int): Unit = {
    while ((!haveReachedEndOfIn) && postReplaceBuffer.size < nBytesRequested) {
      val readResult = preReplaceBuffer.loadFromInputStream(in)
      haveReachedEndOfIn = readResult.haveReachedEndOfInputStream
      replacerMap.attemptMatch(preReplaceBuffer) match {
        case MatchesBeginning(nBytesToReplace, replacement) =>
          postReplaceBuffer.add(replacement)
          preReplaceBuffer.dropFirstNBytes(nBytesToReplace)
        case NoMatchForFirstNBytes(nBytes) =>
          postReplaceBuffer.add(preReplaceBuffer.popFirstNBytes(nBytes))
      }
    }
  }
}

object ReplacerInputStream {

  val chunkSizeDefault: Int = 1024
  val skipChunkSize: Int = 4096

  trait ReplacerMap {
    def maxPatternSize: Int

    def attemptMatch(preReplaceButter: PreReplaceBuffer): MatchResult
  }

  class SimpleReplacerMap(map: Map[Array[Byte], Array[Byte]]) extends ReplacerMap {
    private val maxPatternSizeVal: Int = map.keySet.map(_.length).max

    override def maxPatternSize: Int = maxPatternSizeVal

    override def attemptMatch(preReplaceBuffer: PreReplaceBuffer): MatchResult = {
      var matchAtBeginOpt: Option[MatchesBeginning] = None
      val mapIter = map.iterator
      while (matchAtBeginOpt.isEmpty && mapIter.hasNext) {
        val (pattern, replacement) = mapIter.next()
        if (matchesBeginning(preReplaceBuffer, pattern)) {
          matchAtBeginOpt = Some(MatchesBeginning(pattern.length, replacement))
        }
      }
      matchAtBeginOpt match {
        case Some(matchesBeginning) => matchesBeginning
        case None =>
          var pos: Int = 1
          val patterns = map.keys
          while (!possibleMatchAt(preReplaceBuffer, pos, patterns)) {
            pos += 1
          }
          NoMatchForFirstNBytes(pos)
      }
    }

    private def matchesBeginning(buffer: PreReplaceBuffer, pattern: Array[Byte]): Boolean = {
      if (pattern.length <= buffer.nBytesStored) {
        var isMatchingSoFar: Boolean = true
        var i = 0
        while (isMatchingSoFar && i < pattern.length) {
          isMatchingSoFar = pattern(i) == buffer.bytes(i)
          i += 1
        }
        isMatchingSoFar
      } else {
        false
      }
    }

    private def possibleMatchAt(buffer: PreReplaceBuffer, pos: Int, patterns: Iterable[Array[Byte]]): Boolean = {
      var foundPossibleMatch: Boolean = false
      val patternIter = patterns.iterator
      while ((!foundPossibleMatch) && patternIter.hasNext) {
        val pattern = patternIter.next()
        var isMatchingSoFar: Boolean = true
        var i: Int = 0
        while (i < pattern.length && (pos + i) < buffer.nBytesStored) {
          isMatchingSoFar &&= buffer.bytes(pos + i) == pattern(i)
          i += 1
        }
        foundPossibleMatch ||= isMatchingSoFar
      }
      foundPossibleMatch
    }
  }

  object ReplacerMap {
    def fromBytesMap(map: Map[Array[Byte], Array[Byte]]): SimpleReplacerMap = new SimpleReplacerMap(map)

    sealed trait MatchResult

    case class MatchesBeginning(nBytesToReplace: Int, replacement: Array[Byte]) extends MatchResult

    case class NoMatchForFirstNBytes(nBytes: Int) extends MatchResult

  }

  class PreReplaceBuffer(val bytes: Array[Byte], var nBytesStored: Int) {
    def nBytesFree: Int = bytes.length - nBytesStored

    def loadFromInputStream(in: InputStream): ReadResult = {
      var nBytesLoaded: Int = 0
      var haveReachedEndOfInputStream: Boolean = false
      var nBytesToLoad: Int = nBytesFree
      while (nBytesToLoad > 0 && !haveReachedEndOfInputStream) {
        nBytesLoaded = in.read(bytes, nBytesStored, nBytesToLoad)
        if (nBytesLoaded > 0) {
          nBytesStored += nBytesLoaded
        } else {
          haveReachedEndOfInputStream = true
        }
        nBytesToLoad = nBytesFree
      }
      ReadResult(nBytesLoaded, haveReachedEndOfInputStream)
    }

    def dropFirstNBytes(nBytes: Int): Unit = {
      System.arraycopy(bytes, nBytes, bytes, 0, nBytesStored)
      nBytesStored -= nBytes
    }

    def popFirstNBytes(nBytes: Int): Array[Byte] = {
      val firstNBytes = util.Arrays.copyOf(bytes, nBytes)
      dropFirstNBytes(nBytes)
      firstNBytes
    }
  }

  object PreReplaceBuffer {
    def apply(size: Int): PreReplaceBuffer = new PreReplaceBuffer(new Array[Byte](size), 0)

    case class ReadResult(nBytesRead: Int, haveReachedEndOfInputStream: Boolean)

  }

  class PostReplaceBuffer(var byteArrays: Seq[Array[Byte]], var offset: Int = 0) {
    def size: Int = byteArrays.map(_.length).sum

    def add(byteArray: Array[Byte]): Unit = {
      if(byteArray.nonEmpty) {
        byteArrays :+= byteArray
      }
    }

    def popOneByte(): Int = {
      if(byteArrays.isEmpty) {
        -1
      } else {
        val byteArraysHead = byteArrays.head
        val firstByte = byteArraysHead(offset)
        offset += 1
        if(offset == byteArraysHead.length) {
          byteArrays = byteArrays.tail
          offset = 0
        }
        firstByte
      }
    }

    def popBytesChunk(buffer: Array[Byte], buffOff: Int, nBytesRequested: Int): Int = {
      val nBytesTarget = Math.min(nBytesRequested, buffer.length - buffOff)
      val byteArraysHead = byteArrays.head
      val nBytesInHead = byteArraysHead.length - offset
      if(nBytesTarget < nBytesInHead) {
        System.arraycopy(byteArraysHead, offset, buffer, buffOff, nBytesTarget)
        offset += nBytesTarget
        nBytesTarget
      } else {
        System.arraycopy(byteArraysHead, offset, buffer, buffOff, nBytesInHead)
        byteArrays = byteArrays.tail
        offset = 0
        nBytesInHead
      }
    }

    def popBytes(buffer: Array[Byte], buffOff: Int, len: Int): Int = {
      if(len == 0) {
        0
      } else if(byteArrays.isEmpty) {
        -1
      } else {
        var nBytesPopped: Int = 0
        val nBytesPoppedTarget = Math.min(len, buffer.length - buffOff)
        while(byteArrays.nonEmpty && nBytesPopped < nBytesPoppedTarget) {
          nBytesPopped += popBytesChunk(buffer, buffOff + nBytesPopped, nBytesPoppedTarget - nBytesPopped)
        }
        nBytesPopped
      }
    }

    def dropBytesChunk(nBytesToDrop: Int): Int = {
      val bytesArraysHead = byteArrays.head
      val nBytesInHead = bytesArraysHead.length - offset
      if(nBytesToDrop < nBytesInHead) {
        offset += nBytesToDrop
        nBytesToDrop
      } else {
        byteArrays = byteArrays.tail
        offset = 0
        nBytesInHead
      }
    }

    def dropBytes(nBytesToDrop: Int): Int = {
      var nBytesDropped: Int = 0
      while(byteArrays.nonEmpty && nBytesDropped < nBytesToDrop) {
        nBytesDropped += dropBytesChunk(nBytesToDrop - nBytesDropped)
      }
      nBytesDropped
    }
  }

  object PostReplaceBuffer {
    def newEmpty(): PostReplaceBuffer = new PostReplaceBuffer(Seq.empty)
  }

}