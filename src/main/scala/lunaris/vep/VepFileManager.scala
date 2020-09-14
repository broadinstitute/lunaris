package lunaris.vep

import akka.{Done, NotUsed}
import akka.actor.ActorSystem
import akka.stream.{IOResult, Materializer}
import akka.stream.scaladsl.{FileIO, Source}
import akka.util.ByteString
import better.files.File
import lunaris.data.BlockGzippedWithIndex
import lunaris.io.ResourceConfig
import lunaris.recipes.eval.{LunCompiler, LunRunContext}
import lunaris.utils.NumberParser
import lunaris.vep.VepFileManager.{ResultId, ResultStatus}
import org.broadinstitute.yootilz.core.snag.Snag

import scala.concurrent.{ExecutionContextExecutor, Future}
import scala.util.control.NonFatal
import scala.util.{Failure, Random, Success}

class VepFileManager(val inputsFolder: File, val resultsFolder: File,
                     val dataFileWithIndex: BlockGzippedWithIndex, val varId: String,
                     resourceConfig: ResourceConfig) {

  var statusById: Map[ResultId, ResultStatus] = Map.empty

  def updateStatus(resultId: ResultId, resultStatus: ResultStatus): Unit = {
    statusById = statusById + (resultId -> resultStatus)
  }

  def folderOrSnag(folder: File, folderNick: String): Either[Snag, File] = {
    if (folder.exists && !folder.isDirectory) {
      Left(Snag(s"$folder should be folder, but is not."))
    } else if (!folder.exists) {
      folder.createDirectories()
      if (folder.exists) {
        Right(folder)
      } else {
        Left(Snag(s"Failed to create $folderNick $folder"))
      }
    } else {
      Right(resultsFolder)
    }
  }

  def resultsFolderOrSnag(): Either[Snag, File] = folderOrSnag(resultsFolder, "results folder")

  def inputsFolderOrSnag(): Either[Snag, File] = folderOrSnag(inputsFolder, "inputs folder")

  def foldersExistOrSnag(): Either[Snag, Unit] = {
    for {
      _ <- inputsFolderOrSnag()
      _ <- resultsFolderOrSnag()
    } yield ()
  }

  def createNewIdFor(inputFileName: String): ResultId = ResultId.createNew(inputFileName)

  def inputFileNameForId(resultId: ResultId): String = "input_" + resultId.toString

  def inputFilePathForId(resultId: ResultId): File = inputsFolder / inputFileNameForId(resultId)

  def outputFileNameForId(resultId: ResultId): String = resultId.toString

  def outputFilePathForId(resultId: ResultId): File = resultsFolder / outputFileNameForId(resultId)

  def uploadFile(stream: Source[ByteString, Any], inputFile: File)(
    implicit actorSystem: ActorSystem
  ): Future[IOResult] = stream.runWith(FileIO.toPath(inputFile.path))

  def newQueryFuture(formData: VepFormData)(implicit actorSystem: ActorSystem): Future[Done] = {
    implicit val executionContext: ExecutionContextExecutor = actorSystem.dispatcher
    val resultId = formData.resultId
    val inputFile = inputFilePathForId(resultId)
    val chromsAndRegionsFut = VcfStreamVariantsReader.readChromsAndRegions(inputFile)
    val queryFuture = chromsAndRegionsFut.map { chromsAndRegions =>
      val chroms = chromsAndRegions.chroms
      val regionsByChrom = chromsAndRegions.regions
      val request =
        VepRequestBuilder.buildRequest(
          resultId, chroms, regionsByChrom, inputFile.toString, dataFileWithIndex.data.toString,
          outputFilePathForId(resultId), formData.filter, Some(dataFileWithIndex.index.toString), varId
        )
      LunCompiler.compile(request)
    }.collect {
      case Right(runnable) =>
        val context = {
          LunRunContext(Materializer(actorSystem), resourceConfig, LunRunContext.Observer.forLogger(println))
        }
        runnable.executeAsync(context)
    }.flatten
    queryFuture.onComplete {
      case Success(_) => updateStatus(resultId, ResultStatus.createSucceeded(outputFileNameForId(resultId)))
      case Failure(exception) =>
        val snag = Snag(exception)
        println(snag.report)
        updateStatus(resultId, ResultStatus.createFailed(snag.message))
    }
    queryFuture
  }

  def newUploadAndQueryFutureFuture(formData: VepFormData)(
    implicit actorSystem: ActorSystem): Future[Done] = {
    implicit val executionContext: ExecutionContextExecutor = actorSystem.dispatcher
    newQueryFuture(formData)
  }

  class SubmissionResponse(val resultId: ResultId, val fut: Future[Done])

  def submit(formData: VepFormData)(implicit actorSystem: ActorSystem): SubmissionResponse = {
    implicit val executionContext: ExecutionContextExecutor = actorSystem.dispatcher
    val resultId = formData.resultId
    updateStatus(resultId, ResultStatus.createSubmitted())
    val fut = newUploadAndQueryFutureFuture(formData)
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

object VepFileManager {

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