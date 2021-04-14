package lunaris.vep

import akka.actor.ActorSystem
import akka.http.scaladsl.model.Multipart
import akka.util.ByteString
import better.files.File
import lunaris.expressions.LunBoolExpression
import lunaris.recipes.parsing.LunBoolExpressionParser
import lunaris.vep.VepJobManager.{JobId, SessionId}
import org.broadinstitute.yootilz.core.snag.SnagUtils

import scala.concurrent.{ExecutionContextExecutor, Future}

case class VepFormData(jobId: JobId,
                       inputFileClient: File,
                       inputFileServer: File,
                       filterString: String,
                       filter: LunBoolExpression,
                       format: String,
                       sessionId: SessionId,
                       emailOpt: Option[String])

object VepFormData {
  def fromFields(fields: Map[String, FormField]): VepFormData = {
    val inputFileField = fields(FormField.Keys.inputFile).asInstanceOf[InputFileField]
    val jobId = inputFileField.jobId
    val inputFileClient = inputFileField.inputFileClient
    val inputFileServer = inputFileField.inputFileServer
    val filterString = fields(FormField.Keys.filter).asInstanceOf[FilterField].filter
    val filter = SnagUtils.assertNotSnag(LunBoolExpressionParser.parse(filterString))
    val format = fields(FormField.Keys.format).asInstanceOf[FormatField].format
    val sessionId = fields(FormField.Keys.session).asInstanceOf[SessionIdField].sessionId
    val emailOpt = fields.get(FormField.Keys.email).map(_.asInstanceOf[EmailField]).map(_.email)
    VepFormData(jobId, inputFileClient, inputFileServer, filterString, filter, format, sessionId, emailOpt)
  }

  sealed trait FormField {
    def name: String
  }

  case class FilterField(filter: String) extends FormField {
    override def name: String = FormField.Keys.filter
  }

  case class InputFileField(jobId: JobId, inputFileClient: File, inputFileServer: File)
    extends FormField {
    override def name: String = FormField.Keys.inputFile
  }

  case class FormatField(format: String) extends FormField {
    override def name: String = FormField.Keys.format
  }

  case class SessionIdField(sessionId: SessionId) extends FormField {
    override def name: String = FormField.Keys.session
  }

  case class EmailField(email: String) extends FormField {
    override def name: String = FormField.Keys.email
  }

  case class IgnoredField(name: String) extends FormField

  object FormField {

    object Keys {
      val filter: String = "filter"
      val inputFile: String = "inputFile"
      val format: String = "format"
      val session: String = "session"
      val email: String = "email"
    }

    private def bodyPartToStringFut(bodyPart: Multipart.FormData.BodyPart)(
      implicit actorSystem: ActorSystem, executionContext: ExecutionContextExecutor): Future[String] = {
      bodyPart.entity.dataBytes.runFold(ByteString.empty)(_ ++ _).map(_.utf8String)
    }

    def bodyPartToFieldFut(bodyPart: Multipart.FormData.BodyPart, vepFileManager: VepJobManager)(
      implicit actorSystem: ActorSystem): Future[FormField] = {
      implicit val executionContext: ExecutionContextExecutor = actorSystem.dispatcher
      bodyPart.name match {
        case Keys.filter =>
          bodyPartToStringFut(bodyPart).map(FilterField)
        case Keys.inputFile =>
          val jobId = JobId.createNew()
          val inputFileClient = File(bodyPart.filename.get)
          val inputFileServer = vepFileManager.vepFolders.vepJobFiles(jobId).inputFile
          vepFileManager.uploadFile(bodyPart.entity.dataBytes, inputFileServer).map { _ =>
            InputFileField(jobId, inputFileClient, inputFileServer)
          }
        case Keys.format =>
          bodyPartToStringFut(bodyPart).map(FormatField)
        case Keys.session =>
          bodyPartToStringFut(bodyPart).map(SessionId(_)).map(SessionIdField)
        case Keys.email =>
          bodyPartToStringFut(bodyPart).map(EmailField)
        case unknownName: String =>
          bodyPart.entity.dataBytes.runFold(())((_, _) => ()).map(_ => IgnoredField(unknownName))
      }

    }
  }

}