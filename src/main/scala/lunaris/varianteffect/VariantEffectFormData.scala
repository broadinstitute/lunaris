package lunaris.varianteffect

import akka.actor.ActorSystem
import akka.http.scaladsl.model.Multipart
import akka.util.ByteString
import lunaris.expressions.BooleanRecordExpression
import lunaris.genomics.Variant
import lunaris.recipes.parsing.RecordExpressionParser
import lunaris.varianteffect.VepFileManager.ResultId
import org.broadinstitute.yootilz.core.snag.SnagUtils

import scala.concurrent.{ExecutionContextExecutor, Future}

case class VariantEffectFormData(fileName: String,
                                 resultId: ResultId,
                                 variantsByChrom: Map[String, Seq[Variant]],
                                 filter: BooleanRecordExpression)

object VariantEffectFormData {
  def fromFields(fields: Map[String, FormField]): VariantEffectFormData = {
    val inputFileField = fields(FormField.Keys.inputFile).asInstanceOf[InputFileField]
    val fileName = inputFileField.fileName
    val resultId = inputFileField.resultId
    val variantsByChrom = inputFileField.variantsByChrom
    val filterString = fields(FormField.Keys.filter).asInstanceOf[FilterField].filter
    val filter = SnagUtils.assertNotSnag(RecordExpressionParser.parse(filterString))
    VariantEffectFormData(fileName, resultId, variantsByChrom, filter)
  }

  sealed trait FormField {
    def name: String
  }

  case class FilterField(filter: String) extends FormField {
    override def name: String = FormField.Keys.filter
  }

  case class InputFileField(fileName: String,
                            resultId: VepFileManager.ResultId,
                            variantsByChrom: Map[String, Seq[Variant]]) extends FormField {
    override def name: String = FormField.Keys.inputFile
  }

  case class IgnoredField(name: String) extends FormField

  object FormField {

    object Keys {
      val filter: String = "filter"
      val inputFile: String = "inputFile"
    }

    def bodyPartToFieldFut(bodyPart: Multipart.FormData.BodyPart, vepFileManager: VepFileManager)(
      implicit actorSystem: ActorSystem): Future[FormField] = {
      implicit val executionContext: ExecutionContextExecutor = actorSystem.dispatcher
      bodyPart.name match {
        case Keys.filter =>
          bodyPart.entity.dataBytes.runFold(ByteString.empty)(_ ++ _).map(_.utf8String).map(FilterField)
        case Keys.inputFile =>
          val resultId = vepFileManager.createNewIdFor(bodyPart.filename.get)
          val inputFile = vepFileManager.inputFilePathForId(resultId)
          VcfStreamVariantsReader.newVariantsByChromFuture(bodyPart.entity.dataBytes, inputFile).map { variantsByChrom =>
            InputFileField(bodyPart.filename.get, resultId, variantsByChrom)
          }
        case unknownName: String =>
          bodyPart.entity.dataBytes.runFold(())((_, _) => ()).map(_ => IgnoredField(unknownName))
      }

    }
  }

}