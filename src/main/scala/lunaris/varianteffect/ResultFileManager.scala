package lunaris.varianteffect

import akka.NotUsed
import akka.actor.ActorSystem
import akka.stream.Materializer
import akka.stream.scaladsl.Source
import akka.util.ByteString
import better.files.File
import lunaris.genomics.Variant
import lunaris.io.ResourceConfig
import lunaris.recipes.eval.{LunCompiler, LunRunContext}
import lunaris.utils.NumberParser
import lunaris.varianteffect.ResultFileManager.{ResultId, ResultStatus}
import org.broadinstitute.yootilz.core.snag.Snag

import scala.concurrent.{ExecutionContextExecutor, Future}
import scala.util.control.NonFatal
import scala.util.{Failure, Random, Success}

class ResultFileManager(val resultFolder: File) {

  var statusById: Map[ResultId, ResultStatus] = Map.empty

  def updateStatus(resultId: ResultId, resultStatus: ResultStatus): Unit = {
    statusById = statusById + (resultId -> resultStatus)
  }

  def resultsFolderOrSnag(): Either[Snag, File] = {
    if (resultFolder.exists && !resultFolder.isDirectory) {
      Left(Snag(s"$resultFolder should be folder, but is not."))
    } else if (!resultFolder.exists) {
      resultFolder.createDirectory()
      if (resultFolder.exists) {
        Right(resultFolder)
      } else {
        Left(Snag(s"Failed to create $resultFolder"))
      }
    } else {
      Right(resultFolder)
    }
  }

  def outputFileNameForId(resultId: ResultId): String = resultId.toString

  def outputFilePathForId(resultId: ResultId): File = resultFolder / outputFileNameForId(resultId)

  def newQueryFuture(resultId: ResultId,
                     formData: VariantEffectFormData)(implicit actorSystem: ActorSystem): Future[Unit] = {
    implicit val executionContext: ExecutionContextExecutor = actorSystem.dispatcher
    val queryFuture = Future {
      val request =
        VariantEffectRequestBuilder.buildRequest(resultId, formData.variantsByChrom, outputFilePathForId(resultId))
      LunCompiler.compile(request)
    }.collect {
      case Right(runnable) =>
        val context =
          LunRunContext(Materializer(actorSystem), ResourceConfig.empty, LunRunContext.Observer.forLogger(println))
        runnable.execute(context)
    }
    queryFuture.onComplete {
      case Success(_) => updateStatus(resultId, ResultStatus.createSucceeded(outputFileNameForId(resultId)))
      case Failure(exception) => updateStatus(resultId, ResultStatus.createFailed(exception.getMessage))
    }
    queryFuture
  }

  def newUploadAndQueryFutureFuture(resultId: ResultId, formData: VariantEffectFormData)(
                                     implicit actorSystem: ActorSystem): Future[Unit] = {
    implicit val executionContext: ExecutionContextExecutor = actorSystem.dispatcher
    newQueryFuture(resultId, formData)
  }

  class SubmissionResponse(val resultId: ResultId, val fut: Future[Unit])

  def submit(formData: VariantEffectFormData)(implicit actorSystem: ActorSystem): SubmissionResponse = {
    implicit val executionContext: ExecutionContextExecutor = actorSystem.dispatcher
    val resultId = ResultId.createNew(formData.fileName)
    updateStatus(resultId, ResultStatus.createSubmitted())
    val fut = newUploadAndQueryFutureFuture(resultId, formData)
    new SubmissionResponse(resultId, fut)
  }

  def getStatus(resultId: ResultId): ResultStatus = {
    statusById.getOrElse(resultId, ResultStatus.createUnknown())
  }

  def streamResults(resultId: ResultId): Either[Snag, Source[ByteString, NotUsed]] = {
    val outputFile = outputFilePathForId(resultId)
    try {
      Right(Source.fromIterator(() => outputFile.lineIterator).map(line => ByteString(line + "\n")))
    } catch {
      case NonFatal(ex) => Left(Snag(ex))
    }
  }
}

object ResultFileManager {

  case class ResultId(inputFileName: String, randomPart: Long, timestamp: Long) {
    override def toString: String = inputFileName + "_" + randomPart + "_" + timestamp
  }

  object ResultId {
    def createNew(inputFileName: String): ResultId =
      ResultId(inputFileName, 1 + Random.nextLong(Long.MaxValue), System.currentTimeMillis())

    def fromString(string: String): Either[Snag, ResultId] = {
      val parts = string.split("_")
      if (parts.length < 3) {
        Left(Snag(s"$string is not a valid result id."))
      } else {
        for {
          randomPart <- NumberParser.parseLong(parts(parts.length - 2))
          timestamp <- NumberParser.parseLong(parts(parts.length - 1))
          inputFileName = parts.slice(0, parts.length - 2).mkString("_")
        } yield ResultId(inputFileName, randomPart, timestamp)
      }
    }
  }

  sealed trait ResultStatus {
    def statusCode: Int

    def isSubmitted: Boolean

    def isCompleted: Boolean

    def hasSucceeded: Boolean

    def hasFailed: Boolean

    def timestamp: Long

    def message: String

    def outputFileOpt: Option[String]
  }

  object ResultStatus {

    case class Invalid(timestamp: Long, message: String) extends ResultStatus {
      override def statusCode: Int = -19

      override def isSubmitted: Boolean = false

      override def isCompleted: Boolean = false

      override def hasSucceeded: Boolean = false

      override def hasFailed: Boolean = false

      override def outputFileOpt: Option[String] = None
    }

    case class Unknown(timestamp: Long) extends ResultStatus {
      override def statusCode: Int = -1

      override def isSubmitted: Boolean = false

      override def isCompleted: Boolean = false

      override def hasSucceeded: Boolean = false

      override def hasFailed: Boolean = false

      override def message: String = "Unknown id."

      override def outputFileOpt: Option[String] = None
    }

    case class Submitted(timestamp: Long) extends ResultStatus {
      override def statusCode: Int = 1

      override def isSubmitted: Boolean = true

      override def isCompleted: Boolean = false

      override def hasSucceeded: Boolean = false

      override def hasFailed: Boolean = false

      override def message: String = "Submitted"

      override def outputFileOpt: Option[String] = None
    }

    case class Succeeded(timestamp: Long, outputFile: String) extends ResultStatus {
      override def statusCode: Int = 7

      override def isSubmitted: Boolean = true

      override def isCompleted: Boolean = true

      override def hasSucceeded: Boolean = true

      override def hasFailed: Boolean = false

      override def message: String = s"Success!"

      override def outputFileOpt: Option[String] = Some(outputFile)
    }

    case class Failed(timestamp: Long, message: String) extends ResultStatus {
      override def statusCode: Int = 19

      override def isSubmitted: Boolean = true

      override def isCompleted: Boolean = true

      override def hasSucceeded: Boolean = false

      override def hasFailed: Boolean = true

      override def outputFileOpt: Option[String] = None
    }

    def createInvalid(message: String): Invalid = Invalid(System.currentTimeMillis(), message)

    def createUnknown(): Unknown = Unknown(System.currentTimeMillis())

    def createSubmitted(): Submitted = Submitted(System.currentTimeMillis())

    def createSucceeded(outputFile: String): Succeeded = Succeeded(System.currentTimeMillis(), outputFile)

    def createFailed(message: String): Failed = Failed(System.currentTimeMillis(), message)
  }

}