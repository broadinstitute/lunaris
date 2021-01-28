package lunaris.vep

import akka.actor.ActorSystem
import akka.http.scaladsl.model.Multipart
import akka.util.ByteString
import better.files.File
import lunaris.expressions.LunBoolExpression
import lunaris.recipes.parsing.LunBoolExpressionParser
import lunaris.vep.VepFileManager.SessionId
import lunaris.vep.db.EggDb.JobRecord
import org.broadinstitute.yootilz.core.snag.SnagUtils

import scala.concurrent.{ExecutionContextExecutor, Future}

case class VepFormData(fileName: String,
                       job: JobRecord,
                       filter: LunBoolExpression,
                       format: String,
                       sessionId: SessionId)

object VepFormData {
  def fromFields(fields: Map[String, FormField]): VepFormData = {
    val inputFileField = fields(FormField.Keys.inputFile).asInstanceOf[InputFileField]
    val fileName = inputFileField.fileName
    val job = inputFileField.job
    val filterString = fields(FormField.Keys.filter).asInstanceOf[FilterField].filter
    val filter = SnagUtils.assertNotSnag(LunBoolExpressionParser.parse(filterString))
    val format = fields(FormField.Keys.format).asInstanceOf[FormatField].format
    val sessionId = fields(FormField.Keys.session).asInstanceOf[SessionIdField].sessionId
    VepFormData(fileName, job, filter, format, sessionId)
  }

  sealed trait FormField {
    def name: String
  }

  case class FilterField(filter: String) extends FormField {
    override def name: String = FormField.Keys.filter
  }

  case class InputFileField(fileName: String,
                            job: JobRecord) extends FormField {
    override def name: String = FormField.Keys.inputFile
  }

  case class FormatField(format: String) extends FormField {
    override def name: String = FormField.Keys.format
  }

  case class SessionIdField(sessionId: SessionId) extends FormField {
    override def name: String = FormField.Keys.session
  }

  case class IgnoredField(name: String) extends FormField

  object FormField {

    object Keys {
      val filter: String = "filter"
      val inputFile: String = "inputFile"
      val format: String = "format"
      val session: String = "session"
    }

    private def bodyPartToStringFut(bodyPart: Multipart.FormData.BodyPart)(
      implicit actorSystem: ActorSystem, executionContext: ExecutionContextExecutor): Future[String] = {
      bodyPart.entity.dataBytes.runFold(ByteString.empty)(_ ++ _).map(_.utf8String)
    }

    def bodyPartToFieldFut(bodyPart: Multipart.FormData.BodyPart, vepFileManager: VepFileManager)(
      implicit actorSystem: ActorSystem): Future[FormField] = {
      implicit val executionContext: ExecutionContextExecutor = actorSystem.dispatcher
      bodyPart.name match {
        case Keys.filter =>
          bodyPartToStringFut(bodyPart).map(FilterField)
        case Keys.inputFile =>
          val job = vepFileManager.createNewJob(bodyPart.filename.map(File(_)))
          vepFileManager.uploadFile(bodyPart.entity.dataBytes, job.inputFile).map { _ =>
            InputFileField(bodyPart.filename.get, job)
          }
        case Keys.format =>
          bodyPartToStringFut(bodyPart).map(FormatField)
        case Keys.session =>
          bodyPartToStringFut(bodyPart).map(SessionId).map(SessionIdField)
        case unknownName: String =>
          bodyPart.entity.dataBytes.runFold(())((_, _) => ()).map(_ => IgnoredField(unknownName))
      }

    }
  }

}