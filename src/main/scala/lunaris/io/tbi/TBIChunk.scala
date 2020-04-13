package lunaris.io.tbi

case class TBIChunk(begin: TbiVirtualFileOffset, end: TbiVirtualFileOffset) extends Ordered[TBIChunk] {
  override def compare(that: TBIChunk): Int = {
    val compareBegin = begin.compare(that.begin)
    if(compareBegin != 0) {
      compareBegin
    } else {
      end.compare(that.end)
    }
  }
}
