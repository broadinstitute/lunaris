package lunaris.io.tbi

case class TbiVirtualFileOffset(offsetOfBlock: Long, offsetInBlock: Long)

object TbiVirtualFileOffset {
  def apply(long: Long): TbiVirtualFileOffset = TbiVirtualFileOffset(long >>> 16, long & 65535)
}
