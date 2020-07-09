package lunaris.varianteffect

import java.util.concurrent.TimeUnit

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

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContextExecutor}
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

  def scheduleCalculation(resultId: ResultId,
                          stream: Source[ByteString, Any])(implicit actorSystem: ActorSystem): Unit = {
    val variantsByChromFut = stream.via(Framing.delimiter(ByteString("/n"), Int.MaxValue, allowTruncation = true))
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
      }.runFold(Map.empty[String, Set[Variant]]) { (variantsByChrom, variant) =>
      val chrom = variant.chrom
      val variantsForChrom = variantsByChrom.getOrElse(chrom, Set.empty)
      val variantsForChromNew = variantsForChrom + variant
      variantsByChrom + (chrom -> variantsForChromNew)
    }
    implicit val executionContext: ExecutionContextExecutor = actorSystem.dispatcher
    variantsByChromFut.map { variantsByChrom =>
      val request =
        VariantEffectRequestBuilder.buildRequest(resultId, variantsByChrom, outputFilePathForId(resultId))
      LunCompiler.compile(request)
    }.collect {
      case Right(runnable) =>
        val context =
          LunRunContext(Materializer(actorSystem), ResourceConfig.empty, LunRunContext.Observer.forLogger(println))
        runnable.execute(context)
    }.onComplete {
      case Success(_) => updateStatus(resultId, ResultStatus.createSucceeded(outputFileNameForId(resultId)))
      case Failure(exception) => updateStatus(resultId, ResultStatus.createFailed(exception.getMessage))
    }
  }

  def submit(fileName: String, stream: Source[ByteString, Any])(implicit actorSystem: ActorSystem): ResultId = {
    val resultId = ResultId.createNew(fileName)
    scheduleCalculation(resultId, stream)
    resultId
  }

  def getStatus(resultId: ResultId): ResultStatus = {
    statusById.getOrElse(resultId, ResultStatus.createUnknown())
  }

  def streamResults(resultId: ResultId): Either[Snag, Source[ByteString, NotUsed]] = {
    val outputFile = outputFilePathForId(resultId)
    try {
      Right(Source.fromIterator(() => outputFile.lineIterator).map(ByteString(_)))
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
      ResultId(inputFileName, Random.nextLong(), System.currentTimeMillis())

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
    def isSubmitted: Boolean

    def isCompleted: Boolean

    def hasSucceeded: Boolean

    def hasFailed: Boolean

    def timestamp: Long

    def message: String

    def outputFileOpt: Option[String]
  }

  object ResultStatus {

    case class Unknown(timestamp: Long) extends ResultStatus {
      override def isSubmitted: Boolean = false

      override def isCompleted: Boolean = false

      override def hasSucceeded: Boolean = false

      override def hasFailed: Boolean = false

      override def message: String = "Unknown id."

      override def outputFileOpt: Option[String] = None
    }

    case class Submitted(timestamp: Long) extends ResultStatus {
      override def isSubmitted: Boolean = true

      override def isCompleted: Boolean = false

      override def hasSucceeded: Boolean = false

      override def hasFailed: Boolean = false

      override def message: String = "Submitted"

      override def outputFileOpt: Option[String] = None
    }

    case class Succeeded(timestamp: Long, outputFile: String) extends ResultStatus {
      override def isSubmitted: Boolean = true

      override def isCompleted: Boolean = true

      override def hasSucceeded: Boolean = true

      override def hasFailed: Boolean = false

      override def message: String = s"Success! Output file is $outputFile"

      override def outputFileOpt: Option[String] = Some(outputFile)
    }

    case class Failed(timestamp: Long, message: String) extends ResultStatus {
      override def isSubmitted: Boolean = true

      override def isCompleted: Boolean = true

      override def hasSucceeded: Boolean = false

      override def hasFailed: Boolean = true

      override def outputFileOpt: Option[String] = None
    }

    def createUnknown(): Unknown = Unknown(System.currentTimeMillis())

    def createSubmitted(): Submitted = Submitted(System.currentTimeMillis())

    def createSucceeded(outputFile: String): Succeeded = Succeeded(System.currentTimeMillis(), outputFile)

    def createFailed(message: String): Failed = Failed(System.currentTimeMillis(), message)
  }

}