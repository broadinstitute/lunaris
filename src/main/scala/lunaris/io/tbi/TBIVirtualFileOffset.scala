package lunaris.io.tbi

case class TBIVirtualFileOffset(offsetOfBlock: Long, offsetInBlock: Int) extends Ordered[TBIVirtualFileOffset] {
  override def compare(that: TBIVirtualFileOffset): Int = {
    val diffOfBlock = offsetOfBlock - that.offsetOfBlock
    if (diffOfBlock < 0) {
      -1
    } else if (diffOfBlock > 0) {
      1
    } else {
      offsetInBlock - that.offsetInBlock
    }
  }
}

object TBIVirtualFileOffset {
  val startOfFile: TBIVirtualFileOffset = TBIVirtualFileOffset(0, 0)
  val endOfFile: TBIVirtualFileOffset = TBIVirtualFileOffset(Long.MaxValue, Int.MaxValue)

  def apply(long: Long): TBIVirtualFileOffset = TBIVirtualFileOffset(long >>> 16, (long & 65535).toInt)
}
