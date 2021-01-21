package musha

import better.files.File
import lunaris.vep.VepFileManager
import musha.sql.SqlName.Table
import musha.sql.{Sql, SqlColumn, SqlColumnValue, SqlType}
import musha.map.FieldExtractors._
import musha.map.FunBuilder._
import org.broadinstitute.yootilz.core.snag.Snag
import org.scalatest.funsuite.AnyFunSuite

import java.sql.ResultSet

class MushaTest extends AnyFunSuite {

  def assertRight[A](either: Either[Snag, A]): Unit = {
    assert(either.isRight, either.left.toOption.map(_.report).getOrElse(""))
  }

  def runShowTables(musha: Musha): Unit = {
    val query = MushaQuery.rowsIter[String](Sql.ShowTables) { rs =>
      rs.getString(1)
    }
    val snagOrUnit = musha.runQuery(query) { iter =>
      println(s"Column count: ${iter.metaData.columns.size}")
      println(s"Column names: ${iter.metaData.columnNames.mkString("(", ", ", ")")}.")
      println(s"Row count: ${iter.size}")
    }
    assertRight(snagOrUnit)
  }

  def runCreateTable(musha: Musha, table: Table, columns: SqlColumn[_]*): Unit = {
    val query = MushaQuery.update(Sql.createTableIfNotExists(table, columns))
    println(query.sql.sqlString)
    assertRight(musha.runUpdate(query))
  }

  def runInsert(musha: Musha, table: Table, columnValues: SqlColumnValue[_]*): Unit = {
    val insert = MushaQuery.update(Sql.insert(table, columnValues: _*))
    assertRight(musha.runUpdate(insert))
  }

  def runDropTable(musha: Musha, table: Table): Unit = {
    val drop = MushaQuery.update(Sql.dropTable(table))
    assertRight(musha.runUpdate(drop))
  }

  val extractor: F3[ResultSet, String, String, String, (String, String, String)] =
    (str(1) & str(2) & str(3)) ((_, _, _))

  val consumer: Iterator[(String, String, String)] => Unit =
    (rowsIter: Iterator[(String, String, String)]) =>
      rowsIter.foreach { case (id, in, out) => println(s"$id\t$in\t$out") }

  def runSelect(musha: Musha, table: Table): Unit = {
    val select = MushaQuery.rowsIter(Sql.select(table))(extractor)
    val snagOrResult = musha.runQuery(select)(consumer)
    assertRight(snagOrResult)
  }

  def runSelectWhere[A](musha: Musha, table: Table, column: SqlColumn[A], value: A): Unit = {
    val select = MushaQuery.rowsIter(Sql.select(table, Sql.Equals(column, value)))(extractor)
    val snagOrResult = musha.runQuery(select)(consumer)
    assertRight(snagOrResult)
  }

  test("Hello") {
    val config = MushaConfig("/home/oliverr/lunaris/vep/work/h2/egg", "egg", "armeritter")
    val musha = new Musha(config)
    runShowTables(musha)
    val filesTable = Sql.table("files")
    val idColumn = Sql.column("job_id", SqlType.Varchar(8)).asPrimaryKey
    val inputFileColumn = Sql.column("input_file", SqlType.Varchar(256))
    val outputFileColumn = Sql.column("output_file", SqlType.Varchar(256))
    runCreateTable(musha, filesTable, idColumn, inputFileColumn, outputFileColumn)
    runShowTables(musha)
    val jobId = VepFileManager.ResultId.createNew()
    val inputFile = File("input.vcf")
    val outputFile = File(jobId.string + ".tsv")
    runInsert(musha, filesTable, idColumn.withValue(jobId.string), inputFileColumn.withValue(inputFile.toString),
      outputFileColumn.withValue(outputFile.toString))
    runSelect(musha, filesTable)
    runSelectWhere(musha, filesTable, idColumn, jobId.string)
    runDropTable(musha, filesTable)
    musha.close()
  }
}
