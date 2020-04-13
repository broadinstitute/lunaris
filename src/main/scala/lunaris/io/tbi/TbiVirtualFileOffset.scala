package lunaris.io.tbi

case class TbiVirtualFileOffset(offsetOfBlock: Long, offsetInBlock: Int) extends Ordered[TbiVirtualFileOffset] {
  override def compare(that: TbiVirtualFileOffset): Int = {
    val diffOfBlock = offsetOfBlock - that.offsetOfBlock
    if(diffOfBlock < 0) {
      -1
    } else if(diffOfBlock > 0) {
      1
    } else {
      offsetInBlock - that.offsetInBlock
    }
  }
}

object TbiVirtualFileOffset {
  def apply(long: Long): TbiVirtualFileOffset = TbiVirtualFileOffset(long >>> 16, (long & 65535).toInt)
}
