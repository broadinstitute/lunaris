package lunaris.utils

object Crypt {
  private def shiftPointWithin(codePoint: Int, key: Int, from: Int, to: Int): Int = {
    from + ((codePoint - from + key) % (to - from))
  }

  private def shiftString(string: String, keyIter: Iterator[Int]): String = {
    val codePointIter = string.codePointStepper.iterator
    val stringBuilder = new StringBuilder
    while (codePointIter.hasNext) {
      val codePoint = codePointIter.next()
      val codePointKey = keyIter.next()
      val encryptedCodePoint =
        if (codePoint < 32) {
          codePoint
        } else if (codePoint < 128) {
          shiftPointWithin(codePoint, codePointKey, 32, 128)
        } else if(codePoint < 2048) {
          shiftPointWithin(codePoint, codePointKey, 128, 2048)
        } else {
          shiftPointWithin(codePoint, codePointKey, 2048, 65536)
        }
      stringBuilder.underlying.appendCodePoint(encryptedCodePoint)
    }
    stringBuilder.toString
  }
}
