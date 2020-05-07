package lunaris.io.request

import io.circe.generic.auto._
import io.circe.parser.decode
import org.broadinstitute.yootilz.core.snag.Snag
import io.circe.syntax._

object RequestJson {

  def parse(string: String): Either[Snag, Request] = {
    decode[Request](string).left.map(Snag(_))
  }

  def serialize(request: Request): String = request.asJson.toString

}
