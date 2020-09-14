package lunaris.vep

import io.circe.{Encoder, Json}
import lunaris.vep.VepFileManager.ResultStatus

object VepJson {

  implicit val resultStatusEncoder: Encoder[ResultStatus] = (status: ResultStatus) => Json.obj(
    "statusCode" -> Json.fromInt(status.statusCode),
    "submitted" -> Json.fromBoolean(status.isSubmitted),
    "completed" -> Json.fromBoolean(status.isCompleted),
    "succeeded" -> Json.fromBoolean(status.hasSucceeded),
    "failed" -> Json.fromBoolean(status.hasFailed),
    "message" -> Json.fromString(status.message)
  )

  def resultStatusToJson(resultStatus: ResultStatus): Json = resultStatusEncoder(resultStatus)
}
