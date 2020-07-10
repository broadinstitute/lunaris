package lunaris.varianteffect

import io.circe.{Encoder, Json}
import lunaris.varianteffect.ResultFileManager.ResultStatus

object VariantEffectJson {

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
