package lunaris.io.request

import io.circe.Decoder.Result
import io.circe.{Decoder, HCursor}
import io.circe.generic.auto._
import io.circe.parser.decode
import org.broadinstitute.yootilz.core.snag.Snag
import io.circe.syntax._
import lunaris.streams.tools.ToolCall

object RequestJson {

  def parse(string: String): Either[Snag, Request] = {
    decode[Request](string).left.map(Snag(_))
  }

  def serialize(request: Request): String = request.asJson.toString

  implicit val toolCallDecoder: Decoder[ToolCall] = (cursor: HCursor) => {
    ???
  }

}
