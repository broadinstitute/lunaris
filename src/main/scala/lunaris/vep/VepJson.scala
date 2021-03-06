package lunaris.vep

import io.circe.{Encoder, Json}
import lunaris.vep.VepJobManager.ResultStatus
import lunaris.vep.db.EggDb.{JobIdClientFile, SessionRecord}
import org.broadinstitute.yootilz.core.snag.Snag

object VepJson {

  implicit val resultStatusEncoder: Encoder[ResultStatus] = (status: ResultStatus) => Json.obj(
    "statusType" -> Json.fromString(status.statusType.toString),
    "submitted" -> Json.fromBoolean(status.statusType.isSubmitted),
    "completed" -> Json.fromBoolean(status.statusType.isCompleted),
    "succeeded" -> Json.fromBoolean(status.statusType.hasSucceeded),
    "failed" -> Json.fromBoolean(status.statusType.hasFailed),
    "message" -> Json.fromString(status.message),
    "snagMessages" -> Json.fromValues(status.snagMessages.map(Json.fromString))
  )

  implicit val jobIdClientFileEncoder: Encoder[JobIdClientFile] = (jobIdClientFile: JobIdClientFile) => Json.obj(
    "id" -> Json.fromString(jobIdClientFile.id.string),
    "inputFile" -> Json.fromString(jobIdClientFile.clientFile.toString)
  )

  implicit val sessionEncoder: Encoder[SessionRecord] = (session: SessionRecord) => Json.obj(
    "found" -> Json.fromBoolean(true),
    "error" -> Json.fromBoolean(false),
    "id" -> Json.fromString(session.id.string),
    "jobs" -> Json.fromValues(session.jobIdsAndFiles.map(jobIdClientFileEncoder(_))),
    "filter" -> Json.fromString(session.filter),
    "format" -> Json.fromString(session.format),
    "ctime" -> Json.fromLong(session.cTime),
    "mtime" -> Json.fromLong(session.mTime)
  )

  implicit val snagOrSessionOptEncoder: Encoder[Either[Snag, Option[SessionRecord]]] = {
    case Left(snag) =>
      Json.obj(
        "found" -> Json.fromBoolean(false),
        "error" -> Json.fromBoolean(true),
        "message" -> Json.fromString(snag.message),
        "report" -> Json.fromString(snag.report)
      )
    case Right(None) =>
      Json.obj(
        "found" -> Json.fromBoolean(false),
        "error" -> Json.fromBoolean(false),
        "message" -> Json.fromString("Session not found")
      )
    case Right(Some(session)) =>
      sessionEncoder(session)
  }

  def resultStatusToJson(resultStatus: ResultStatus): Json = resultStatusEncoder(resultStatus)
}
