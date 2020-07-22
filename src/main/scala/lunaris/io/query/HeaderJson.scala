package lunaris.io.query

import io.circe.syntax._
import io.circe.{Encoder, Json}
import lunaris.streams.TsvHeader

object HeaderJson {

  implicit val headerEncoder: Encoder[TsvHeader] = (header: TsvHeader) => Json.obj(
    "col_names" -> header.colNames.asJson
  )

}
