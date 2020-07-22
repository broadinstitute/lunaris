package lunaris.utils

import io.circe.syntax._
import io.circe.{Encoder, Json}
import org.broadinstitute.yootilz.core.snag.Snag

object SnagJson {

  implicit val snagEncoder: Encoder[Snag] = (snag: Snag) => Json.obj(
    "isError" -> true.asJson,
    "message" -> snag.message.asJson,
    "report" -> snag.report.asJson
  )

}
