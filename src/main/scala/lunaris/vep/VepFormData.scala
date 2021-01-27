package lunaris.vep

import akka.actor.ActorSystem
import akka.http.scaladsl.model.Multipart
import akka.util.ByteString
import better.files.File
import lunaris.expressions.LunBoolExpression
import lunaris.recipes.parsing.LunBoolExpressionParser
import lunaris.vep.VepFileManager.ResultId
import lunaris.vep.db.EggDb.JobRecord
import org.broadinstitute.yootilz.core.snag.SnagUtils

import scala.concurrent.{ExecutionContextExecutor, Future}

case class VepFormData(fileName: String,
                       job: JobRecord,
                       filter: LunBoolExpression,
                       format: String)

object VepFormData {
  def fromFields(fields: Map[String, FormField]): VepFormData = {
    val inputFileField = fields(FormField.Keys.inputFile).asInstanceOf[InputFileField]
    val fileName = inputFileField.fileName
    val job = inputFileField.job
    val filterString = fields(FormField.Keys.filter).asInstanceOf[FilterField].filter
    val filter = SnagUtils.assertNotSnag(LunBoolExpressionParser.parse(filterString))
    val format = fields(FormField.Keys.format).asInstanceOf[FormatField].format
    VepFormData(fileName, job, filter, format)
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

  case class IgnoredField(name: String) extends FormField

  object FormField {

    object Keys {
      val filter: String = "filter"
      val inputFile: String = "inputFile"
      val format: String = "format"
    }

    def bodyPartToFieldFut(bodyPart: Multipart.FormData.BodyPart, vepFileManager: VepFileManager)(
      implicit actorSystem: ActorSystem): Future[FormField] = {
      implicit val executionContext: ExecutionContextExecutor = actorSystem.dispatcher
      bodyPart.name match {
        case Keys.filter =>
          bodyPart.entity.dataBytes.runFold(ByteString.empty)(_ ++ _).map(_.utf8String).map(FilterField)
        case Keys.inputFile =>
          val job = vepFileManager.createNewJob(bodyPart.filename.map(File(_)))
          vepFileManager.uploadFile(bodyPart.entity.dataBytes, job.inputFile).map { _ =>
            InputFileField(bodyPart.filename.get, job)
          }
        case Keys.format =>
          bodyPart.entity.dataBytes.runFold(ByteString.empty)(_ ++ _).map(_.utf8String).map(FormatField)
        case unknownName: String =>
          bodyPart.entity.dataBytes.runFold(())((_, _) => ()).map(_ => IgnoredField(unknownName))
      }

    }
  }

}