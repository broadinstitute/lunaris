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
  private val messageNumberCol = Sql.column("message_number", SqlType.SqlInt)
  private val messageCol = Sql.column("message", SqlType.Varchar(128))
  private val jobsTable = Sql.table("files", idCol, inputFileCol, outputFileCol, messageCol)
  private val messagesTable = Sql.table("messages", messageNumberCol, messageCol)

  createTablesIfNotExist()

  def createTablesIfNotExist(): Unit = {
    rightOrThrow(musha.runUpdate(MushaQuery.update(Sql.createTableIfNotExists(jobsTable))))
    rightOrThrow(musha.runUpdate(MushaQuery.update(Sql.createTableIfNotExists(messagesTable))))
  }

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