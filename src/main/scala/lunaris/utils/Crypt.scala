package lunaris.utils

import lunaris.utils.Crypt.{RandomIntIter, shiftString}

import scala.util.Random

case class Crypt(key: String) {

  private def shift(string: String, isForward: Boolean): String = {
    val intIter = new RandomIntIter(Mousse(key).asRandom, isForward)
    shiftString(string, intIter)
  }

  def encrypt(string: String): String = shift(string, isForward = true)

  def decrypt(string: String): String = shift(string, isForward = false)
}

object Crypt {

  class RandomIntIter(random: Random, isForward: Boolean) extends Iterator[Int] {
    override def hasNext: Boolean = true

    override def next(): Int = {
      if (isForward) {
        random.nextInt()
      } else {
        -random.nextInt()
      }
    }
  }

  private def positiveModulo(x: Int, y: Int): Int = ((x % y) + y) % y

  private def shiftPointWithin(codePoint: Int, key: Int, from: Int, to: Int): Int = {
    from + positiveModulo(codePoint - from + key, to - from)
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
        } else if (codePoint < 2048) {
          shiftPointWithin(codePoint, codePointKey, 128, 2048)
        } else {
          shiftPointWithin(codePoint, codePointKey, 2048, 65536)
        }
      stringBuilder.underlying.appendCodePoint(encryptedCodePoint)
    }
    stringBuilder.toString
  }
}
