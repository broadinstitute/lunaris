package lunaris.vep

import akka.NotUsed
import akka.actor.ActorSystem
import akka.stream.scaladsl.{FileIO, Source}
import akka.stream.{IOResult, Materializer}
import akka.util.ByteString
import better.files.File
import lunaris.app.{VepDataFieldsSettings, VepSettings}
import lunaris.data.BlockGzippedWithIndex
import lunaris.io.ResourceConfig
import lunaris.recipes.eval.LunRunnable.RunResult
import lunaris.recipes.eval.{LunCompiler, LunRunContext, SnagTracker}
import lunaris.utils.DateUtils
import lunaris.vep.VepFileManager.{ResultId, ResultStatus}
import lunaris.vep.db.EggDb
import lunaris.vep.db.EggDb.JobRecord
import org.broadinstitute.yootilz.core.snag.{Snag, SnagException}

import scala.concurrent.{ExecutionContextExecutor, Future}
import scala.util.control.NonFatal
import scala.util.{Failure, Random, Success}

class VepFileManager(val vepSettings: VepSettings, resourceConfig: ResourceConfig) {

  val inputsFolder: File = vepSettings.inputsFolder
  val resultsFolder: File = vepSettings.resultsFolder
  val dataFileWithIndex: BlockGzippedWithIndex = vepSettings.dataFileWithIndex
  val vepDataFields: VepDataFieldsSettings = vepSettings.vepDataFieldsSettings
  val dbFile: File = vepSettings.runSettings.workDir / "db" / "egg"
  val eggDb: EggDb = EggDb(dbFile, inputFilePathForId, outputFilePathForId)

  def reportSnag(snag: Snag): Unit = {
    println(snag.report)
  }

  def updateStatus(resultId: ResultId, resultStatus: ResultStatus): Unit = {
    eggDb.updateJobStatus(resultId, resultStatus) match {
      case Left(snag) => reportSnag(snag)
      case _ => ()
    }
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

  def createNewIdFor(): ResultId = ResultId.createNew()

  def inputFileNameForId(resultId: ResultId): String = "input_" + resultId.string + ".vcf"

  def inputFilePathForId(resultId: ResultId): File = inputsFolder / inputFileNameForId(resultId)

  def outputFileNameForId(resultId: ResultId): String = resultId.string + ".tsv"

  def outputFilePathForId(resultId: ResultId): File = resultsFolder / outputFileNameForId(resultId)

  def uploadFile(stream: Source[ByteString, Any], inputFile: File)(
    implicit actorSystem: ActorSystem
  ): Future[IOResult] = stream.runWith(FileIO.toPath(inputFile.path))

  def newQueryFuture(formData: VepFormData)(
    implicit actorSystem: ActorSystem
  ): Future[RunResult] = {
    implicit val executionContext: ExecutionContextExecutor = actorSystem.dispatcher
    val job = formData.job
    val resultId = job.id
    val inputFile = inputFilePathForId(resultId)
    val chromsAndRegionsFut = VcfStreamVariantsReader.readChromsAndRegions(inputFile)
    val queryFuture = chromsAndRegionsFut.map { chromsAndRegions =>
      val chroms = chromsAndRegions.chroms
      val regionsByChrom = chromsAndRegions.regions
      val request = {
        VepRequestBuilder.buildRequest(
          resultId, chroms, regionsByChrom, inputFile.toString, dataFileWithIndex.data.toString,
          job.outputFile, formData.format, formData.filter, Some(dataFileWithIndex.index.toString),
          vepDataFields
        )
      }
      LunCompiler.compile(request)
    }.collect {
      case Right(runnable) =>
        val context = {
          LunRunContext(Materializer(actorSystem), resourceConfig)
        }
        val snagTracker = SnagTracker.briefConsolePrinting
        runnable.executeAsync(context, snagTracker)
    }.flatten
    queryFuture.onComplete {
      case Success(runResult) =>
        val successTime = System.currentTimeMillis()
        val snagMessages = runResult.snags.map(_.message)
        updateStatus(resultId, ResultStatus.createSucceeded(job.ctime, successTime, snagMessages))
      case Failure(exception) =>
        val failTime = System.currentTimeMillis()
        val snag = Snag(exception)
        println(snag.report)
        updateStatus(resultId, ResultStatus.createFailed(job.ctime, failTime, snag.message, Seq(snag.message)))
    }
    queryFuture
  }

  def newUploadAndQueryFutureFuture(formData: VepFormData)(
    implicit actorSystem: ActorSystem): Future[RunResult] = {
    implicit val executionContext: ExecutionContextExecutor = actorSystem.dispatcher
    newQueryFuture(formData)
  }

  class SubmissionResponse(val resultId: ResultId, val fut: Future[RunResult])

  def createNewJob(inputFileClientOpt: Option[File]): JobRecord = {
    eggDb.newSubmittedJob(inputFileClientOpt) match {
      case Left(snag) => throw new SnagException(snag)
      case Right(job) => job
    }
  }

  def submit(formData: VepFormData)(implicit actorSystem: ActorSystem): SubmissionResponse = {
    implicit val executionContext: ExecutionContextExecutor = actorSystem.dispatcher

    val job = formData.job
    val fut = newUploadAndQueryFutureFuture(formData)
    new SubmissionResponse(job.id, fut)
  }

  def getStatus(resultId: ResultId): ResultStatus = {
    eggDb.getJobOpt(resultId) match {
      case Left(snag) =>
        reportSnag(snag)
        ResultStatus.createUnknown(resultId)
      case Right(Some(job)) => job.status
      case Right(None) => ResultStatus.createUnknown(resultId)
    }
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

  final case class ResultId(string: String) {
    override def toString: String = string
  }

  object ResultId {
    private def fourHexDigits(num: Long): String = ("000" + num.toHexString).takeRight(4)

    private def positiveRandomLong(): Long = {
      val raw = Random.nextLong()
      if(raw < 0) raw + Long.MaxValue else raw
    }

    def createNew(): ResultId =
      ResultId(fourHexDigits(System.currentTimeMillis()) + fourHexDigits(positiveRandomLong()))
  }

  case class ResultStatus(statusType: ResultStatus.Type, message: String, snagMessages: Seq[String])

  object ResultStatus {

    sealed trait Type {
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

      val all: Set[Type] = Set(Invalid, Unknown, Submitted, Succeeded, Failed)

      val stringToType: Map[String, Type] = all.map(tpe => (tpe.toString, tpe)).toMap

      def fromString(string: String): Type = stringToType.getOrElse(string, Invalid)
    }

    def createInvalid(message: String): ResultStatus = ResultStatus(Type.Invalid, message, Seq(message))

    def createUnknown(resultId: ResultId): ResultStatus = {
      val message = s"Unknown submission id $resultId."
      ResultStatus(Type.Unknown, message, Seq(message))
    }

    def createSubmitted(submissionTime: Long, snagMessages: Seq[String]): ResultStatus = {
      val subTimeStr = DateUtils.timeToString(submissionTime)
      ResultStatus(Type.Submitted, s"Submitted at $subTimeStr.", snagMessages)
    }

    def createSucceeded(submissionTime: Long, successTime: Long, snagMessages: Seq[String]): ResultStatus = {
      val successTimeStr = DateUtils.timeToString(successTime)
      val timeDiffStr = DateUtils.timeDiffToString(successTime - submissionTime)
      ResultStatus(Type.Succeeded, s"Success on $successTimeStr, after $timeDiffStr.", snagMessages)
    }

    def createFailed(submissionTime: Long, failTime: Long, message: String, snagMessages: Seq[String]):
    ResultStatus = {
      val successTimeStr = DateUtils.timeToString(failTime)
      val timeDiffStr = DateUtils.timeDiffToString(failTime - submissionTime)
      ResultStatus(Type.Succeeded,
        s"Failed on $successTimeStr, after $timeDiffStr: $message", snagMessages)
    }
  }
}