package lunaris.utils

import io.circe.{ACursor, DecodingFailure}

object CirceUtils {
  def newDecodingFailure(message: String, cursor: ACursor): DecodingFailure =
    DecodingFailure(message, cursor.history)
}
