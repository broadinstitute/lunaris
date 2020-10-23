package lunaris.vep

import io.circe.{Encoder, Json}
import lunaris.vep.VepFileManager.ResultStatus

object VepJson {

  implicit val resultStatusEncoder: Encoder[ResultStatus] = (status: ResultStatus) => Json.obj(
    "statusCode" -> Json.fromInt(status.statusType.statusCode),
    "submitted" -> Json.fromBoolean(status.statusType.isSubmitted),
    "completed" -> Json.fromBoolean(status.statusType.isCompleted),
    "succeeded" -> Json.fromBoolean(status.statusType.hasSucceeded),
    "failed" -> Json.fromBoolean(status.statusType.hasFailed),
    "message" -> Json.fromString(status.message)
  )

  def resultStatusToJson(resultStatus: ResultStatus): Json = resultStatusEncoder(resultStatus)
}
