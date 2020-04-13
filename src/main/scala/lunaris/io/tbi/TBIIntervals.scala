package lunaris.io.tbi

import lunaris.genomics.Region

object TBIIntervals {

  val intervalSize: Int = 16384 //  16384 == 1<<14

  def intervalStart(iInterval: Int): Int = iInterval * intervalSize

  def intervalEnd(iInterval: Int): Int = (iInterval + 1) * intervalSize

  def intervalOverlapsRegion(iInterval: Int, region: Region): Boolean = {
    intervalStart(iInterval) < region.end && intervalEnd(iInterval) > region.start
  }

  def firstOverlappingIntervalFor(region: Region): Int = region.start >> 14

  def trimChunks(chunks: Seq[TBIChunk], offset: TbiVirtualFileOffset): Seq[TBIChunk] = {
    chunks.flatMap { chunk =>
      if (chunk.begin >= offset) {
        Some(chunk)
      } else if (chunk.end > offset) {
        Some(TBIChunk(offset, chunk.end))
      } else {
        None
      }
    }
  }
}
