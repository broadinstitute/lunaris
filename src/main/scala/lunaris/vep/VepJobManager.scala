package lunaris.vep

import akka.NotUsed
import akka.actor.ActorSystem
import akka.stream.scaladsl.{FileIO, Source}
import akka.stream.{IOResult, Materializer}
import akka.util.ByteString
import better.files.File
import lunaris.app.{EmailSettings, VepDataFieldsSettings, VepSettings}
import lunaris.data.BlockGzippedWithIndex
import lunaris.genomics.Region
import lunaris.io.{FileInputId, ResourceConfig}
import lunaris.recipes.eval.LunRunnable.RunResult
import lunaris.recipes.eval.{LunCompiler, LunRunContext, RunTracker, SnagTracker, StatsTracker}
import lunaris.selene.Selene
import lunaris.utils.{DateUtils, DebugUtils, ProcessUtils, SnagUtils}
import lunaris.vep.VepJobManager.{JobId, ResultStatus, SessionId}
import lunaris.vep.db.EggDb
import lunaris.vep.db.EggDb.{JobRecord, SessionRecord}
import org.broadinstitute.yootilz.core.snag.{Snag, SnagException}

import java.io.PrintStream
import java.util.Date
import scala.concurrent.{ExecutionContextExecutor, Future}
import scala.util.Random
import scala.util.control.NonFatal

final class VepJobManager(val vepSettings: VepSettings, emailSettings: EmailSettings, dbName: String,
                          emailApiKey: String, resourceConfig: ResourceConfig) {

  val vepFolders: VepFolders = VepFolders(vepSettings)
  private val scriptRepo = ScriptRepo(vepFolders.workFolder)
  private val vcfSorter = VcfSorter(scriptRepo)
  private val dataFileWithIndex: BlockGzippedWithIndex = vepSettings.dataFileWithIndex
  private val vepDataFields: VepDataFieldsSettings = vepSettings.vepDataFieldsSettings
  private val dbFile: File = vepSettings.runSettings.workDir / "db" / dbName
  private val eggDb: EggDb = EggDb(dbFile)
  private val emailManager: EmailManager = new EmailManager(emailSettings, emailApiKey)
  private val exonsFile: File = vepSettings.runSettings.exonsFile

  def reportSnag(snag: Snag): Unit = {
    println(snag.report)
  }

  def updateStatus(jobId: JobId, resultStatus: ResultStatus): Unit = {
    eggDb.updateJobStatus(jobId, resultStatus) match {
      case Left(snag) => reportSnag(snag)
      case _ => ()
    }
  }

  def uploadFile(stream: Source[ByteString, Any], inputFile: File)(
    implicit actorSystem: ActorSystem
  ): Future[IOResult] = stream.runWith(FileIO.toPath(inputFile.path))

  def insertNewJobToDb(id: JobId, sessionId: SessionId, inputFileClient: File, inputFileServer: File,
                       outputFile: File, filter: String, outputFormat: String, submissionTime: Long): Unit = {
    val resultStatus = ResultStatus.createSubmitted(submissionTime, Seq.empty)
    val job = JobRecord(id, sessionId, inputFileClient, inputFileServer, outputFile, filter, outputFormat,
      resultStatus.statusType, resultStatus.message, resultStatus.snagMessages, submissionTime, submissionTime)
    eggDb.insertJob(job) match {
      case Left(snag) => println(snag.report)
      case _ => ()
    }
  }

  private def ls(file: File): Unit = {
    val date = new Date(System.currentTimeMillis())
    println(s"At $date, checking for $file.")
    println(ProcessUtils.ls(file).fold(_.getMessage, printOut => printOut))
  }

  def newQueryFuture(formData: VepFormData)(
    implicit actorSystem: ActorSystem
  ): Future[RunResult] = {
    implicit val executionContext: ExecutionContextExecutor = actorSystem.dispatcher
    val jobId = formData.jobId
    val inputFileServer = formData.inputFileServer
    val vepJobFiles = vepFolders.vepJobFiles(jobId)
    val outputFile = vepJobFiles.outputFile
    val submissionTime = System.currentTimeMillis()

    insertNewJobToDb(jobId, formData.sessionId, formData.inputFileClient, inputFileServer, outputFile,
      formData.filterString, formData.format, submissionTime)
    DebugUtils.printlnDebug("Before inputPreparationFut")
    val inputPreparationFut: Future[Unit] = Future {
      vcfSorter.sortVcf(inputFileServer, inputFileServer)
      Selene.tabix(inputFileServer, dataFileWithIndex.data.asInstanceOf[FileInputId].file, None,
        vepDataFields.ref, vepDataFields.alt, vepJobFiles.extractedDataFile, vepJobFiles.cacheMissesFile)
    }.map (SnagUtils.throwIfSnag)
    DebugUtils.printlnDebug("Before queryFuture")
    val queryFuture = inputPreparationFut.flatMap { _ =>
      val chroms = SnagUtils.throwIfSnag(Selene.readChromosomeList(vepJobFiles.extractedDataFile))
      val coverAllRegion = Region(0, Int.MaxValue)
      val regionsByChrom = chroms.map((_, Seq(coverAllRegion))).toMap
      val requestBuilder =
        VepRequestBuilder(
          jobId, vepJobFiles, chroms, regionsByChrom, exonsFile, vepDataFields, formData.filter, formData.format
        )
      val requestPhaseOne = requestBuilder.buildPhaseOneRequest()
      val snagTracker = SnagTracker.briefConsolePrinting
      LunCompiler.compile(requestPhaseOne) match {
        case Left(snag) =>
          snagTracker.trackSnag(snag)
          Future.failed(new SnagException(snag))
        case Right(runnableOne) =>
          val context = LunRunContext(Materializer(actorSystem), resourceConfig)
          val out = new PrintStream(vepFolders.vepJobFiles(jobId).logFile.newFileOutputStream(append = true))
          val statsTracker = StatsTracker(out.println)
          val runTracker = RunTracker(snagTracker, statsTracker)
          runnableOne.executeAsync(context, runTracker).flatMap { runResultOne =>
            val vepRunner = VepRunner.createNewVepRunner(vepFolders, scriptRepo, vepSettings.runSettings)
            ls(vepJobFiles.vepInputFile)
            val vepReturnValue =
              vepRunner.runVep(vepJobFiles.vepInputFile, vepJobFiles.vepOutputFile, vepJobFiles.logFile)
            ls(vepJobFiles.vepInputFile)
            ls(vepJobFiles.vepOutputFile)
            if (vepReturnValue != 0) {
              val snag = Snag(s"VEP return value should be zero, but was $vepReturnValue.")
              snagTracker.trackSnag(snag)
              Future.failed(new SnagException(snag))
            } else {
              val requestPhaseTwo = requestBuilder.buildPhaseTwoRequest()
              LunCompiler.compile(requestPhaseTwo) match {
                case Left(snag) =>
                  snagTracker.trackSnag(snag)
                  Future.failed(new SnagException(snag))
                case Right(runnableTwo) =>
                  runnableTwo.executeAsync(context, runTracker).map(runResultOne ++ _)
              }
            }
          }
      }
    }
    val successTransform: RunResult => RunResult = { runResult =>
      val successTime = System.currentTimeMillis()
      val successDate = new Date(successTime)
      println(s"$jobId succeeded at $successDate.")
      val snagMessages = runResult.snags.map(_.message)
      updateStatus(jobId, ResultStatus.createSucceeded(submissionTime, successTime, snagMessages))
      formData.emailOpt.foreach { email =>
        emailManager.sendJobResultMessage(email, submissionTime, successTime, jobId, formData.sessionId, runResult)
      }
      runResult
    }
    val exceptionTransform: Throwable => Throwable = { exception =>
      val failTime = System.currentTimeMillis()
      val failDate = new Date(failTime)
      println(s"$jobId failed at $failDate")
      val snag = Snag(exception)
      println(snag.report)
      updateStatus(jobId, ResultStatus.createFailed(submissionTime, failTime, snag.message, Seq(snag.message)))
      formData.emailOpt.foreach { email =>
        emailManager.sendJobSnagMessage(email, submissionTime, failTime, formData.sessionId, snag)
      }
      exception
    }
    DebugUtils.printlnDebug("Before queryFuture.transform()")
    queryFuture.transform(successTransform, exceptionTransform)
  }

  def newUploadAndQueryFutureFuture(formData: VepFormData)(
    implicit actorSystem: ActorSystem): Future[RunResult] = {
    implicit val executionContext: ExecutionContextExecutor = actorSystem.dispatcher
    newQueryFuture(formData)
  }

  class SubmissionResponse(val jobId: JobId, val fut: Future[RunResult])

  def submit(formData: VepFormData)(implicit actorSystem: ActorSystem): SubmissionResponse = {
    implicit val executionContext: ExecutionContextExecutor = actorSystem.dispatcher
    DebugUtils.printlnDebug("Before newUploadAndQueryFutureFuture")
    val fut = newUploadAndQueryFutureFuture(formData)
    DebugUtils.printlnDebug("After newUploadAndQueryFutureFuture")
    val submissionResponse = new SubmissionResponse(formData.jobId, fut)
    DebugUtils.printlnDebug("After new SubmissionResponse(formData.jobId, fut)")
    submissionResponse
  }

  def getStatus(resultId: JobId): ResultStatus = {
    eggDb.getJobOpt(resultId) match {
      case Left(snag) =>
        reportSnag(snag)
        ResultStatus.createUnknown(resultId)
      case Right(Some(job)) => job.status
      case Right(None) => ResultStatus.createUnknown(resultId)
    }
  }

  def getSession(sessionId: SessionId): Either[Snag, Option[SessionRecord]] = {
    val snagOrSessionOpt = eggDb.getSessionOpt(sessionId)
    snagOrSessionOpt.left.foreach(reportSnag)
    snagOrSessionOpt
  }

  def streamResults(jobId: JobId): Either[Snag, Source[ByteString, NotUsed]] = {
    val outputFile = vepFolders.vepJobFiles(jobId).outputFile
    try {
      Right(Source.fromIterator(() => outputFile.lineIterator).map(line => ByteString(line + "\n")))
    } catch {
      case NonFatal(ex) => Left(Snag(ex))
    }
  }
}

object VepJobManager {

  def fourHexDigits(num: Long): String = ("000" + num.toHexString).takeRight(4)

  private def positiveRandomLong(): Long = {
    val raw = Random.nextLong()
    if (raw < 0) raw + Long.MaxValue else raw
  }

  def eightHexDigits(): String = fourHexDigits(System.currentTimeMillis()) + fourHexDigits(positiveRandomLong())

  final case class JobId(string: String) {
    override def toString: String = string
  }

  object JobId {
    def createNew(): JobId = JobId(eightHexDigits())
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

    def createUnknown(resultId: JobId): ResultStatus = {
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

  final case class SessionId(string: String) {
    override def toString: String = string
  }

  object SessionId {
    def createNew(): SessionId = SessionId(eightHexDigits())
  }
}