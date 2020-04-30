package lunaris.utils

import java.util.regex.Pattern

import org.broadinstitute.yootilz.core.snag.Snag

import scala.util.control.NonFatal

object NumberParser {
  val intRegex: Pattern = Pattern.compile("^([+\\-])?\\d+$")

  def parseInt(string: String): Either[Snag, Int] = {
    if (intRegex.matcher(string).matches()) {
      try {
        Right(string.toInt)
      } catch {
        case NonFatal(ex) => Left(Snag(s"Could not parse $string as Int", Snag(ex)))
      }
    } else {
      Left(Snag(s"Could not parse $string as Int"))
    }
  }
}
