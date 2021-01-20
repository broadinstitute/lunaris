package musha

import lunaris.vep.VepFileManager
import musha.sql.SqlName.Table
import musha.sql.{Sql, SqlColumn, SqlColumnValue, SqlType}
import org.broadinstitute.yootilz.core.snag.Snag
import org.scalatest.funsuite.AnyFunSuite

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

  def runCreateTable(musha: Musha, table: Table, column: SqlColumn[_]): Unit = {
    val query =
      MushaQuery.update(Sql.createTableIfNotExists(table, Seq(column)))
    println(query.sql.sqlString)
    assertRight(musha.runUpdate(query))
  }

  def runInsert(musha: Musha, table: Table, columnValues: SqlColumnValue[_]): Unit = {
    val insert = MushaQuery.update(Sql.insert(table, columnValues))
    assertRight(musha.runUpdate(insert))
  }

  def runDropTable(musha: Musha, table: Table): Unit = {
    val drop = MushaQuery.update(Sql.dropTable(table))
    assertRight(musha.runUpdate(drop))
  }

  test("Hello") {
    val config = MushaConfig("/home/oliverr/lunaris/vep/work/h2/egg", "egg", "armeritter")
    val musha = new Musha(config)
    runShowTables(musha)
    val table = Sql.table("Jobs")
    val column = Sql.column("jobId", SqlType.Varchar(20)).asPrimaryKey
    runCreateTable(musha, table, column)
    runShowTables(musha)
    val inputFile = "test.vcf"
    val jobId = VepFileManager.ResultId.createNew(inputFile)
    val columnValue =  column.withValue(jobId.string)
    runInsert(musha, table, columnValue)
    runDropTable(musha, table)
    musha.close()
  }
}
