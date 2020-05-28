package lunaris.utils

import java.util.regex.Pattern

import org.broadinstitute.yootilz.core.snag.Snag

import scala.util.control.NonFatal

object NumberParser {
  val intPattern: Pattern = Pattern.compile("^([+\\-])?\\d+$")
  val floatPattern: Pattern = Pattern.compile("[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?")

  def parseAs[T](string: String)(pattern: Pattern, name: String)(converter: String => T): Either[Snag, T] = {
    if (pattern.matcher(string).matches()) {
      try {
        Right(converter(string))
      } catch {
        case NonFatal(ex) => Left(Snag(s"Could not parse $string as $name.", Snag(ex)))
      }
    } else {
      Left(Snag(s"Could not parse $string as $name."))
    }
  }

  def parseInt(string: String): Either[Snag, Int] = parseAs[Int](string)(intPattern, "Int")(_.toInt)

  def parseLong(string: String): Either[Snag, Long] = parseAs[Long](string)(intPattern, "Long")(_.toLong)

  def parseFloat(string: String): Either[Snag, Float] = parseAs[Float](string)(floatPattern, "Float")(_.toFloat)

  def parseDouble(string: String): Either[Snag, Double] =
    parseAs[Double](string)(floatPattern, "Double")(_.toDouble)
}
