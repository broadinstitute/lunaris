package lunaris.utils

import java.io.{FilterInputStream, IOException, InputStream}
import java.util

import lunaris.utils.ReplacerInputStream.ReplacerMap
import lunaris.utils.ReplacerInputStream.ReplacerMap.MatchResult

class ReplacerInputStream(in: InputStream, replacerMap: ReplacerMap, minChunkSize: Int) extends FilterInputStream(in) {

  val targetChunkSize: Int = Math.max(replacerMap.maxPatternSize, minChunkSize)


  override def read(): Int = ???

  override def read(b: Array[Byte]): Int = ???

  override def read(b: Array[Byte], off: Int, len: Int): Int = ???

  override def skip(n: Long): Long = ???

  override def available(): Int = ???

  override def mark(readlimit: Int): Unit = throw new IOException("ReplacerInputStream does not support mark().")

  override def reset(): Unit = throw new IOException("ReplacerInputStream does not support mark().")

  override def markSupported(): Boolean = false

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
      var matchAtBeginOpt: Option[Array[Byte]] = None
      for ((pattern, replacement) <- map) {
        ???
      }
      ???
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

    def matchesBeginning(pattern: Array[Byte]): Boolean = {
      if (pattern.length <= nBytesStored) {
        var matchesSoFar: Boolean = true
        var i = 0
        while (matchesSoFar && i < pattern.length) {
          matchesSoFar = pattern(i) == bytes(i)
          i += 1
        }
        matchesSoFar
      } else {
        false
      }
    }

    def partialMatchAt(patterns: Iterable[Array[Byte]]): Boolean = ???

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

  class PostReplaceBuffer(var byteArrays: Seq[Array[Byte]]) {
    def size: Int = byteArrays.map(_.length).sum
  }

}