package lunaris.utils

import java.io.{FilterInputStream, IOException, InputStream}
import java.util

import lunaris.utils.ReplacerInputStream.{PostReplaceBuffer, PreReplaceBuffer, ReplacerMap}
import lunaris.utils.ReplacerInputStream.ReplacerMap.{MatchResult, MatchesBeginning, NoMatchForFirstNBytes}

class ReplacerInputStream(in: InputStream, replacerMap: ReplacerMap, minChunkSize: Int) extends FilterInputStream(in) {

  private val targetChunkSize: Int = Math.max(replacerMap.maxPatternSize, minChunkSize)
  private val preReplaceBuffer: PreReplaceBuffer = PreReplaceBuffer(targetChunkSize)
  private val postReplaceBuffer: PostReplaceBuffer = PostReplaceBuffer.newEmpty()

  override def read(): Int = ???

  override def read(b: Array[Byte]): Int = ???

  override def read(b: Array[Byte], off: Int, len: Int): Int = ???

  override def skip(n: Long): Long = ???

  override def available(): Int = ???

  override def mark(readlimit: Int): Unit = throw new IOException("ReplacerInputStream does not support mark().")

  override def reset(): Unit = throw new IOException("ReplacerInputStream does not support mark().")

  override def markSupported(): Boolean = false

  private def tryToMakeAvailable(nBytesRequested: Int): Unit = {
    val underlyingExhausted: Boolean = false
    while((!underlyingExhausted) && postReplaceBuffer.size < nBytesRequested) {
      ???
    }
  }

}

object ReplacerInputStream {

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
      while(matchAtBeginOpt.isEmpty && mapIter.hasNext) {
        val (pattern, replacement) = mapIter.next()
        if(matchesBeginning(preReplaceBuffer, pattern)) {
          matchAtBeginOpt = Some(MatchesBeginning(pattern.length, replacement))
        }
      }
      matchAtBeginOpt match {
        case Some(matchesBeginning) => matchesBeginning
        case None =>
          var pos: Int = 1
          val patterns = map.keys
          while(!possibleMatchAt(preReplaceBuffer, pos, patterns)) {
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
      while((!foundPossibleMatch) && patternIter.hasNext) {
        val pattern = patternIter.next()
        var isMatchingSoFar: Boolean = true
        var i: Int = 0
        while(i < pattern.length && (pos+ i) < buffer.nBytesStored) {
          isMatchingSoFar &&= buffer.bytes(pos+i) == pattern(i)
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

    def loadFromInputStream(in: InputStream): Int = {
      var nBytesLoaded: Int = 0
      var failedToRead: Boolean = false
      var nBytesToLoad: Int = nBytesFree
      while (nBytesToLoad > 0 && !failedToRead) {
        nBytesLoaded = in.read(bytes, nBytesStored, nBytesToLoad)
        if (nBytesLoaded > 0) {
          nBytesStored += nBytesLoaded
        } else {
          failedToRead = true
        }
        nBytesToLoad = nBytesFree
      }
      nBytesLoaded
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
  }

  class PostReplaceBuffer(var byteArrays: Seq[Array[Byte]]) {
    def size: Int = byteArrays.map(_.length).sum
  }

  object PostReplaceBuffer {
    def newEmpty(): PostReplaceBuffer = new PostReplaceBuffer(Seq.empty)
  }
}