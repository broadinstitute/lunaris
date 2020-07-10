package lunaris.varianteffect

import akka.NotUsed
import akka.actor.ActorSystem
import akka.stream.Materializer
import akka.stream.scaladsl.{Framing, Source}
import akka.util.ByteString
import better.files.File
import lunaris.genomics.Variant
import lunaris.io.ResourceConfig
import lunaris.recipes.eval.{LunCompiler, LunRunContext}
import lunaris.utils.NumberParser
import lunaris.varianteffect.ResultFileManager.{ResultId, ResultStatus}
import org.broadinstitute.yootilz.core.snag.Snag

import scala.collection.mutable
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

  def newVariantsByChromFuture(resultId: ResultId,
                               stream: Source[ByteString, Any])(
                                implicit actorSystem: ActorSystem
                              ): Future[Map[String, Seq[Variant]]] = {
    implicit val executionContext: ExecutionContextExecutor = actorSystem.dispatcher

    stream.via(Framing.delimiter(ByteString("\n"), Int.MaxValue))
      .map(_.utf8String)
      .filter(!_.startsWith("#"))
      .mapConcat { line =>
        val fields = line.split("\t")
        if (fields.length >= 5) {
          NumberParser.parseInt(fields(1)) match {
            case Left(_) => Seq.empty
            case Right(pos) =>
              val chrom = fields(0)
              val ref = fields(3)
              val alt = fields(4)
              Seq(Variant(chrom, pos, ref, alt))
          }
        } else {
          Seq.empty
        }
      }.runFold(Map.empty[String, mutable.Builder[Variant, Seq[Variant]]]) { (variantsByChrom, variant) =>
      val chrom = variant.chrom
      variantsByChrom.get(chrom) match {
        case Some(variantsForChrom) =>
          variantsForChrom += variant
          variantsByChrom
        case None =>
          val variantsForChrom = Seq.newBuilder[Variant]
          variantsForChrom += variant
          variantsByChrom + (chrom -> variantsForChrom)
      }
    }.map(_.view.mapValues(_.result()).toMap)
  }

  def newQueryFuture(resultId: ResultId,
                     variantsByChrom: Map[String, Seq[Variant]])(implicit actorSystem: ActorSystem): Future[Unit] = {
    implicit val executionContext: ExecutionContextExecutor = actorSystem.dispatcher
    val queryFuture = Future {
      val request =
        VariantEffectRequestBuilder.buildRequest(resultId, variantsByChrom, outputFilePathForId(resultId))
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

  def newUploadAndQueryFutureFuture(resultId: ResultId,
                                    stream: Source[ByteString, Any])(
                                     implicit actorSystem: ActorSystem): Future[Future[Unit]] = {
    implicit val executionContext: ExecutionContextExecutor = actorSystem.dispatcher
    newVariantsByChromFuture(resultId, stream).map { variantsByChrom =>
      newQueryFuture(resultId, variantsByChrom)
    }
  }

  class SubmissionResponse(val resultId: ResultId, val futFut: Future[Future[Unit]])

  def submit(fileName: String,
             stream: Source[ByteString, Any])(implicit actorSystem: ActorSystem): SubmissionResponse = {
    val resultId = ResultId.createNew(fileName)
    updateStatus(resultId, ResultStatus.createSubmitted())
    val futFut = newUploadAndQueryFutureFuture(resultId, stream)
    new SubmissionResponse(resultId, futFut)
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