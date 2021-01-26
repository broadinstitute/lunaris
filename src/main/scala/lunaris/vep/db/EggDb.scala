package lunaris.vep.db

import better.files.File
import lunaris.vep.VepFileManager.{ResultId, ResultStatus}
import lunaris.vep.db.EggDb.{JobRecord, rightOrThrow}
import musha.map.FunBuilder._
import musha.sql.{Sql, SqlType}
import musha.{Musha, MushaConfig, MushaQuery}
import org.broadinstitute.yootilz.core.snag.Snag

import scala.collection.immutable.ArraySeq

class EggDb(mushaConfig: MushaConfig, outputFileForId: ResultId => File) {
  private val musha = Musha(mushaConfig)
  private val idCol = Sql.column("id", SqlType.Varchar(8)).bimap[ResultId](_.string, ResultId(_))
  private val inputFileCol = Sql.column("input_file", SqlType.Varchar(256)).bimap[File](_.toString, File(_))
  private val outputFileCol = Sql.column("output_file", SqlType.Varchar(128)).bimap[File](_.toString, File(_))
  private val statusTypeCol =
    Sql.column("status_type", SqlType.Varchar(9))
      .bimap[ResultStatus.Type](_.toString, ResultStatus.Type.stringToType)
  private val messageCol = Sql.column("message", SqlType.Varchar(128))
  private val messagesCol =
    Sql.column("messages", SqlType.Clob).bimap[Seq[String]](messagesToString, stringToMessages)
  private val cTimeCol = Sql.column("ctime", SqlType.BigInt)
  private val mTimeCol = Sql.column("mtime", SqlType.BigInt)
  private val jobsTable =
    Sql.table("files", idCol.asPrimaryKey, inputFileCol, outputFileCol, statusTypeCol, messageCol, messagesCol,
      cTimeCol, mTimeCol)

  createTableIfNotExist()

  private def messagesToString(messages: Seq[String]): String = messages.mkString("\n")

  private def stringToMessages(string: String): Seq[String] = {
    if(string.isEmpty) {
      Seq.empty
    } else {
      ArraySeq.unsafeWrapArray(string.split("\n"))
    }
  }

  def createTableIfNotExist(): Unit = {
    rightOrThrow(musha.runUpdate(MushaQuery.update(Sql.createTableIfNotExists(jobsTable))))
  }

  def newSubmittedJob(inputFile: File): Either[Snag, JobRecord] = {
    val id = ResultId.createNew()
    val outputFile = outputFileForId(id)
    val time = System.currentTimeMillis()
    val messages = Seq.empty[String]
    val submittedStatus = ResultStatus.createSubmitted(time, messages)
    val statusType = submittedStatus.statusType
    val message = submittedStatus.message
    val sql = Sql.insert(
      jobsTable, idCol.withValue(id), inputFileCol.withValue(inputFile), outputFileCol.withValue(outputFile),
      statusTypeCol.withValue(statusType), messageCol.withValue(message), messagesCol.withValue(messages),
      cTimeCol.withValue(time), mTimeCol.withValue(time)
    )
    val query = MushaQuery.update(sql)
    musha.runUpdate(query).filterOrElse(_ > 0, Snag("Insert of new record failed")).map { _ =>
      JobRecord(id, inputFile, outputFile, statusType, message, messages, time, time)
    }
  }

  def updateJobStatus(id: ResultId, status: ResultStatus): Either[Snag, Unit] = {
    val mTime = System.currentTimeMillis()
    val sql = Sql.merge(
      jobsTable, idCol.withValue(id), statusTypeCol.withValue(status.statusType),
      messageCol.withValue(status.message), messagesCol.withValue(status.snagMessages), mTimeCol.withValue(mTime)
    )
    val query = MushaQuery.update(sql)
    musha.runUpdate(query).filterOrElse(_ > 0, Snag(s"Update of record $id failed.")).map(_ => ())
  }

  private val rowMapper =
    (idCol.get & inputFileCol.get & outputFileCol.get & statusTypeCol.get & messageCol.get & messagesCol.get
    & cTimeCol.get & mTimeCol.get) (JobRecord)

  def getJob(id: ResultId): Either[Snag, JobRecord] = {
    val sql = Sql.select(jobsTable, Sql.Equals(idCol.sqlColumn, id.string))
    val query = MushaQuery.singleResult(sql)(rowMapper)
    musha.runSingleResultQuery(query)
  }

  def getJobOpt(id: ResultId): Either[Snag, Option[JobRecord]] = {
    val sql = Sql.select(jobsTable, Sql.Equals(idCol.sqlColumn, id.string))
    val query = MushaQuery.optionalResult(sql)(rowMapper)
    musha.runOptionalResultQuery(query)
  }

  def forEachJob(jobConsumer: JobRecord => Any): Either[Snag, Unit] = {
    val sql = Sql.select(jobsTable)
    val query = MushaQuery.rowsIter(sql)(rowMapper)
    musha.runQuery(query)(_.foreach(jobConsumer))
  }

  def countJobs(): Either[Snag, Long] = {
    val sql = Sql.countRows(jobsTable)
    val query = MushaQuery.rowCount(sql)
    musha.runCountRows(query)
  }

  def deleteJob(id: ResultId): Either[Snag, Unit] = {
    val sql = Sql.delete(jobsTable, Sql.Equals(idCol.sqlColumn, id.string))
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

  def close(): Unit = {
    musha.close()
  }
}

object EggDb {

  object Defaults {
    val user = "egg"
    val password = "armeritter"
  }

  def apply(dbFile: File, outputFileForId: ResultId => File): EggDb = {
    new EggDb(MushaConfig(dbFile, Defaults.user, Defaults.password), outputFileForId)
  }

  class EggDbException(message: String) extends Exception(message)

  def rightOrThrow[A](snagOrValue: Either[Snag, A]): A = {
    snagOrValue match {
      case Left(snag) => throw new EggDbException(snag.message)
      case Right(a) => a
    }
  }

  case class JobRecord(id: ResultId, inputFile: File, outputFile: File, statusType: ResultStatus.Type, message: String,
                       messages: Seq[String], ctime: Long, mTime: Long)

}