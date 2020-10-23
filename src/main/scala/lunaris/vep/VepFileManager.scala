package lunaris.vep

import java.util.Date

import akka.{Done, NotUsed}
import akka.actor.ActorSystem
import akka.stream.{IOResult, Materializer}
import akka.stream.scaladsl.{FileIO, Source}
import akka.util.ByteString
import better.files.File
import lunaris.app.{VepDataFieldsSettings, VepSettings}
import lunaris.data.BlockGzippedWithIndex
import lunaris.io.ResourceConfig
import lunaris.recipes.eval.{LunCompiler, LunRunContext}
import lunaris.utils.NumberParser
import lunaris.vep.VepFileManager.{ResultId, ResultStatus}
import org.broadinstitute.yootilz.core.snag.Snag

import scala.concurrent.{ExecutionContextExecutor, Future}
import scala.util.control.NonFatal
import scala.util.{Failure, Random, Success}

class VepFileManager(val vepSettings: VepSettings, resourceConfig: ResourceConfig) {

  val inputsFolder: File = vepSettings.inputsFolder
  val resultsFolder: File = vepSettings.resultsFolder
  val dataFileWithIndex: BlockGzippedWithIndex = vepSettings.dataFileWithIndex
  val vepDataFields: VepDataFieldsSettings = vepSettings.vepDataFieldsSettings

  var statusById: Map[ResultId, ResultStatus] = Map.empty
  var submissionTimes: Map[ResultId, Long] = Map.empty

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
          outputFilePathForId(resultId), formData.filter, Some(dataFileWithIndex.index.toString), vepDataFields
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
      case Success(_) => updateStatus(resultId, ResultStatus.createSucceeded())
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

  def recordSubmissionTime(id: ResultId): Unit = {
  }

  def submit(formData: VepFormData)(implicit actorSystem: ActorSystem): SubmissionResponse = {
    implicit val executionContext: ExecutionContextExecutor = actorSystem.dispatcher
    val resultId = formData.resultId
    val submissionTime = System.currentTimeMillis()
    submissionTimes += (resultId -> submissionTime)
    updateStatus(resultId, ResultStatus.createSubmitted(submissionTime))
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

  case class ResultStatus(statusType: ResultStatus.Type, message: String)

  object ResultStatus {

    sealed trait Type {
      def statusCode: Int =
        this match {
          case Type.Invalid => -19
          case Type.Unknown => -1
          case Type.Submitted => 1
          case Type.Succeeded => 7
          case Type.Failed => 19
        }

      def isSubmitted: Boolean = this == Type.Submitted || this == Type.Succeeded || this == Type.Failed

      def isCompleted: Boolean = this == Type.Succeeded || this == Type.Failed

      def hasSucceeded: Boolean = this == Type.Succeeded

      def hasFailed: Boolean = this == Type.Failed
    }

    object Type {
      case object Invalid extends Type

      case object Unknown extends Type

      case object Submitted extends Type

      case object Succeeded extends Type

      case object Failed extends Type
    }

    def createInvalid(message: String): ResultStatus = ResultStatus(Type.Invalid, message)

    def createUnknown(): ResultStatus = ResultStatus(Type.Unknown, "Unknown submission id.")

    def createSubmitted(submissionTime: Long): ResultStatus =
      ResultStatus(Type.Submitted, "Submitted at " + new Date(submissionTime))

    def createSucceeded(): ResultStatus = ResultStatus(Type.Succeeded, "Success!")

    def createFailed(message: String): ResultStatus = ResultStatus(Type.Failed, message)
  }

}