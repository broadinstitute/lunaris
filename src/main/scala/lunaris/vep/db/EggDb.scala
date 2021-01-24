package lunaris.vep.db

import better.files.File
import lunaris.vep.VepFileManager.{ResultId, ResultStatus}
import lunaris.vep.db.EggDb.{JobRecord, rightOrThrow}
import musha.sql.{Sql, SqlType}
import musha.{Musha, MushaConfig, MushaQuery}
import org.broadinstitute.yootilz.core.snag.Snag
import musha.map.FunBuilder._
import musha.map.FieldExtractors._

class EggDb(mushaConfig: MushaConfig) {
  private val musha = Musha(mushaConfig)
  private val idCol = Sql.column("id", SqlType.Varchar(8))
  private val inputFileCol = Sql.column("input_file", SqlType.Varchar(256))
  private val outputFileCol = Sql.column("output_file", SqlType.Varchar(128))
  private val statusTypeCol = Sql.column("status_type", SqlType.Varchar(9))
  private val messageCol = Sql.column("message", SqlType.Varchar(128))
  private val messagesCol = Sql.column("message", SqlType.Clob)
  private val cTimeCol = Sql.column("ctime", SqlType.BigInt)
  private val mTimeCol = Sql.column("mtime", SqlType.BigInt)
  private val jobsTable =
    Sql.table("files", idCol.asPrimaryKey, inputFileCol, outputFileCol, statusTypeCol, messageCol, messagesCol,
      cTimeCol, mTimeCol)

  createTableIfNotExist()

  private def messagesToString(messages: Seq[String]): String = messages.mkString("\n")
  private def stringToMessages(string: String): Seq[String] = string.split("\n")

  def createTableIfNotExist(): Unit = {
    rightOrThrow(musha.runUpdate(MushaQuery.update(Sql.createTableIfNotExists(jobsTable))))
  }

  def newSubmittedJob(inputFile: File, outputFile: File): Either[Snag, JobRecord] = {
    val id = ResultId.createNew()
    val time = System.currentTimeMillis()
    val messages = Seq.empty[String]
    val submittedStatus = ResultStatus.createSubmitted(time, messages)
    val statusType = submittedStatus.statusType
    val message = submittedStatus.message
    val sql = Sql.insert(
        jobsTable, idCol.withValue(id.toString), inputFileCol.withValue(inputFile.toString),
        outputFileCol.withValue(outputFile.toString), statusTypeCol.withValue(statusType.toString),
        messageCol.withValue(message), messagesCol.withValue(messagesToString(messages))
      )
    val query = MushaQuery.update(sql)
    musha.runUpdate(query).filterOrElse(_ > 0, Snag("Insert of new record failed")).map { _ =>
      JobRecord(id, inputFile, outputFile, statusType, message, messages, time, time)
    }
  }

  def updateJobStatus(id: ResultId, status: ResultStatus): Either[Snag, Unit] = {
    val mTime = System.currentTimeMillis()
    val sql = Sql.merge(
      jobsTable, idCol.withValue(id.string), statusTypeCol.withValue(status.statusType.toString),
      messageCol.withValue(status.message), messagesCol.withValue(messagesToString(status.snagMessages)),
      mTimeCol.withValue(mTime)
    )
    val query = MushaQuery.update(sql)
    musha.runUpdate(query).filterOrElse(_ > 0, Snag(s"Update of record $id failed.")).map( _ => ())
  }

  def getJob(id: ResultId): Either[Snag, JobRecord] = {
    val sql = Sql.select(jobsTable, Sql.Equals(idCol, id.string))
    val query = MushaQuery.rowsIter(sql){ rs =>
      ???  //  TODO field extractors from column definitions
    }
    ???
  }

  def deleteJob(id: ResultId): Either[Snag, Unit] = ???

}

object EggDb {

  object Defaults {
    val user = "egg"
    val password = "armeritter"
  }

  def apply(dbFile: File): EggDb = {
    new EggDb(MushaConfig(dbFile, Defaults.user, Defaults.password))
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