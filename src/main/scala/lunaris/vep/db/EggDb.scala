package lunaris.vep.db

import better.files.File
import lunaris.vep.VepFileManager.ResultId
import lunaris.vep.db.EggDb.rightOrThrow
import musha.sql.{Sql, SqlType}
import musha.{Musha, MushaConfig, MushaQuery}
import org.broadinstitute.yootilz.core.snag.Snag

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

  def createTableIfNotExist(): Unit = {
    rightOrThrow(musha.runUpdate(MushaQuery.update(Sql.createTableIfNotExists(jobsTable))))
  }

  //  TODO
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

  case class FilesRecord(id: ResultId, inputFile: File, outputFile: File)

}