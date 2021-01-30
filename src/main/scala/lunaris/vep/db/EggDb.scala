package lunaris.vep.db

import better.files.File
import lunaris.vep.VepFileManager.{JobId, ResultStatus, SessionId}
import lunaris.vep.db.EggDb.{JobIdClientFile, JobRecord, SessionRecord, rightOrThrow}
import musha.map.FunBuilder._
import musha.sql.{Sql, SqlType}
import musha.{Musha, MushaConfig, MushaQuery}
import org.broadinstitute.yootilz.core.snag.Snag

import java.sql.ResultSet
import scala.collection.immutable.ArraySeq

class EggDb(mushaConfig: MushaConfig) {
  private val musha = Musha(mushaConfig)
  private val jobIdCol = Sql.column("id", SqlType.Varchar(8)).bimap[JobId](_.string, JobId(_))
  private val inputFileClientCol =
    Sql.column("input_file_client", SqlType.Varchar(256)).bimap[File](_.toString, File(_))
  private val inputFileCol = Sql.column("input_file", SqlType.Varchar(256)).bimap[File](_.toString, File(_))
  private val outputFileCol = Sql.column("output_file", SqlType.Varchar(128)).bimap[File](_.toString, File(_))
  private val statusTypeCol =
    Sql.column("status_type", SqlType.Varchar(9))
      .bimap[ResultStatus.Type](_.toString, ResultStatus.Type.stringToType)
  private val messageCol = Sql.column("message", SqlType.Varchar(128))
  private val messagesCol =
    Sql.column("messages", SqlType.Clob).bimap[Seq[String]](stringsToString, stringToStrings)

  private val sessionIdCol =
    Sql.column("session", SqlType.Varchar(8)).bimap[SessionId](_.string, SessionId(_))
  private val jobIdsCol =
    Sql.column("jobs", SqlType.Clob).bimap[Seq[JobId]](jobIdsToString, stringToJobIds)
  private val clientFilesCol =
    Sql.column("client_files", SqlType.Clob).bimap[Seq[File]](filesToString, stringToFiles)

  private val filterCol = Sql.column("filter", SqlType.Clob)
  private val outputFormatCol = Sql.column("format", SqlType.Varchar(10))
  private val cTimeCol = Sql.column("ctime", SqlType.BigInt)
  private val mTimeCol = Sql.column("mtime", SqlType.BigInt)

  private val jobsTable =
    Sql.table("jobs", jobIdCol.asPrimaryKey, sessionIdCol, inputFileClientCol, inputFileCol, outputFileCol,
      filterCol, outputFormatCol, statusTypeCol, messageCol, messagesCol, cTimeCol, mTimeCol)
  private val sessionsTable =
    Sql.table("sessions", sessionIdCol.asPrimaryKey, jobIdsCol, clientFilesCol, filterCol, outputFormatCol,
      cTimeCol, mTimeCol)

  createTablesIfNotExist()

  private def stringsToString(strings: Seq[String]): String = strings.mkString("\n")

  private def stringToStrings(string: String): Seq[String] = {
    if (string.isEmpty) {
      Seq.empty
    } else {
      ArraySeq.unsafeWrapArray(string.split("\n"))
    }
  }

  private def jobIdsToString(jobIds: Seq[JobId]): String = stringsToString(jobIds.map(_.string))

  private def stringToJobIds(string: String): Seq[JobId] = stringToStrings(string).map(JobId.apply)

  private def filesToString(files: Seq[File]): String = stringsToString(files.map(_.toString))

  private def stringToFiles(string: String): Seq[File] = stringToStrings(string).map(File(_))

  def createTablesIfNotExist(): Unit = {
    Seq(jobsTable, sessionsTable).foreach { table =>
      rightOrThrow(musha.runUpdate(MushaQuery.update(Sql.createTableIfNotExists(table))))
    }
  }

  def insertJob(job: JobRecord): Either[Snag, Unit] = {
    for {
      _ <- insertIntoJobTable(job)
      _ <- insertIntoSessionTable(job)
    } yield ()
  }

  private def insertIntoJobTable(job: JobRecord): Either[Snag, Unit] = {
    val sql = Sql.insert(
      jobsTable, jobIdCol.withValue(job.id), sessionIdCol.withValue(job.sessionId),
      inputFileClientCol.withValue(job.inputFileClient), inputFileCol.withValue(job.inputFileServer),
      outputFileCol.withValue(job.outputFile), filterCol.withValue(job.filter),
      outputFormatCol.withValue(job.outputFormat), statusTypeCol.withValue(job.statusType),
      messageCol.withValue(job.message), messagesCol.withValue(job.messages), cTimeCol.withValue(job.ctime),
      mTimeCol.withValue(job.mTime)
    )
    val query = MushaQuery.update(sql)
    musha.runUpdate(query).filterOrElse(_ > 0, Snag("Insert of new record failed")).map(_ => ())
  }

  private def insertIntoSessionTable(job: JobRecord): Either[Snag, Unit] = {
    for {
      sessionOpt <- getSessionOpt(job.sessionId)
      _ <- {
        val session = sessionOpt match {
          case None =>
            SessionRecord(job.sessionId, Seq(JobIdClientFile(job.id, job.inputFileClient)), job.filter,
              job.outputFormat, job.ctime, job.mTime)
          case Some(session) =>
            session.copy(jobIdsAndFiles = session.jobIdsAndFiles :+ JobIdClientFile(job.id, job.inputFileClient),
              filter = job.filter, format = job.outputFormat, mTime = job.mTime)
        }
        mergeSession(session)
      }
    } yield ()
  }


  def updateJobStatus(id: JobId, status: ResultStatus): Either[Snag, Unit] = {
    val mTime = System.currentTimeMillis()
    val sql = Sql.merge(
      jobsTable, jobIdCol.withValue(id), statusTypeCol.withValue(status.statusType),
      messageCol.withValue(status.message), messagesCol.withValue(status.snagMessages), mTimeCol.withValue(mTime)
    )
    val query = MushaQuery.update(sql)
    musha.runUpdate(query).filterOrElse(_ > 0, Snag(s"Update of record $id failed.")).map(_ => ())
  }

  private val rowToJob: ResultSet => JobRecord =
    (jobIdCol.get & sessionIdCol.get & inputFileClientCol.get & inputFileCol.get & outputFileCol.get & filterCol.get
      & outputFormatCol.get & statusTypeCol.get & messageCol.get & messagesCol.get & cTimeCol.get
      & mTimeCol.get) (JobRecord)

  def getJob(id: JobId): Either[Snag, JobRecord] = {
    val sql = Sql.select(jobsTable, Sql.Equals(jobIdCol.sqlColumn, id.string))
    val query = MushaQuery.singleResult(sql)(rowToJob)
    musha.runSingleResultQuery(query)
  }

  def getJobOpt(id: JobId): Either[Snag, Option[JobRecord]] = {
    val sql = Sql.select(jobsTable, Sql.Equals(jobIdCol.sqlColumn, id.string))
    val query = MushaQuery.optionalResult(sql)(rowToJob)
    musha.runOptionalResultQuery(query)
  }

  def forEachJob(jobConsumer: JobRecord => Any): Either[Snag, Unit] = {
    val sql = Sql.select(jobsTable)
    val query = MushaQuery.rowsIter(sql)(rowToJob)
    musha.runQuery(query)(_.foreach(jobConsumer))
  }

  def countJobs(): Either[Snag, Long] = {
    val sql = Sql.countRows(jobsTable)
    val query = MushaQuery.rowCount(sql)
    musha.runCountRows(query)
  }

  def deleteJob(id: JobId): Either[Snag, Unit] = {
    val sql = Sql.delete(jobsTable, Sql.Equals(jobIdCol.sqlColumn, id.string))
    val query = MushaQuery.update(sql)
    musha.runUpdate(query) match {
      case Left(snag) => Left(snag)
      case Right(nRows) =>
        if (nRows == 1) {
          Right(())
        } else {
          Left(Snag(s"When deleting row with id ${id.string}, only one row should match, but deleted $nRows rows."))
        }
    }
  }

  private val rowToSession: ResultSet => SessionRecord =
    (sessionIdCol.get & jobIdsCol.get & clientFilesCol.get & filterCol.get & outputFormatCol.get & cTimeCol.get
      & mTimeCol.get) { (sessionId, jobIds, clientFiles, filter, format, cTime, mTime) =>
      val jobIdsAndFiles = jobIds.zip(clientFiles).map {
        case (jobId, clientFile) => JobIdClientFile(jobId, clientFile)
      }
      SessionRecord(sessionId, jobIdsAndFiles, filter, format, cTime, mTime)
    }

  private def mergeSession(session: SessionRecord): Either[Snag, Int] = {
    val (jobIds, clientFiles) =
      session.jobIdsAndFiles.unzip(jobIdClientFile => (jobIdClientFile.id, jobIdClientFile.clientFile))
    val sql =
      Sql.merge(sessionsTable, sessionIdCol.withValue(session.id), jobIdsCol.withValue(jobIds),
        clientFilesCol.withValue(clientFiles), filterCol.withValue(session.filter),
        outputFormatCol.withValue(session.format), cTimeCol.withValue(session.cTime),
        mTimeCol.withValue(session.mTime))
    val query = MushaQuery.update(sql)
    musha.runUpdate(query)
  }

  def getSessionOpt(id: SessionId): Either[Snag, Option[SessionRecord]] = {
    val sql = Sql.select(sessionsTable, Sql.Equals(sessionIdCol.sqlColumn, id.string))
    val query = MushaQuery.optionalResult(sql)(rowToSession)
    musha.runOptionalResultQuery(query)
  }

  def close(): Unit = {
    musha.close()
  }
}

object EggDb {

  object Defaults {
    val user = "egg"
    val password = "armeritter"
  }

  def apply(dbFile: File, inputFileForId: JobId => File, outputFileForId: JobId => File): EggDb = {
    new EggDb(MushaConfig(dbFile, Defaults.user, Defaults.password))
  }

  class EggDbException(message: String) extends Exception(message)

  def rightOrThrow[A](snagOrValue: Either[Snag, A]): A = {
    snagOrValue match {
      case Left(snag) => throw new EggDbException(snag.message)
      case Right(a) => a
    }
  }

  case class JobRecord(id: JobId, sessionId: SessionId, inputFileClient: File, inputFileServer: File,
                       outputFile: File, filter: String, outputFormat: String, statusType: ResultStatus.Type,
                       message: String, messages: Seq[String], ctime: Long, mTime: Long) {
    def status: ResultStatus = ResultStatus(statusType, message, messages)
  }

  case class JobIdClientFile(id: JobId, clientFile: File)

  case class SessionRecord(id: SessionId, jobIdsAndFiles: Seq[JobIdClientFile], filter: String, format: String,
                           cTime: Long, mTime: Long)

}