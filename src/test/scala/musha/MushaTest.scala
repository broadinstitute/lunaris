package musha

import musha.sql.SqlName.Table
import musha.sql.{Sql, SqlColumn, SqlName, SqlType}
import org.scalatest.funsuite.AnyFunSuite

class MushaTest extends AnyFunSuite {
  def runShowTables(musha: Musha): Unit = {
    val query = MushaQuery.rowsIter[String](Sql.ShowTables) { rs =>
      rs.getString(1)
    }
    val snagOrUnit = musha.runQuery(query) { iter =>
      println(s"Column count: ${iter.metaData.columns.size}")
      println(s"Column names: ${iter.metaData.columnNames.mkString("(", ", ", ")")}.")
      println(s"Row count: ${iter.size}")
    }
    snagOrUnit.left.foreach(snag => println(snag.report))
  }

  def runCreateTable(musha: Musha, table: Table, column: SqlColumn[_]): Unit = {
    val query =
      MushaQuery.update(Sql.createTableIfNotExists(table, Seq(column)))
    println(query.sql.sqlString)
    musha.runUpdate(query)
  }

  def runInsert(musha: Musha, table: Table): Unit = {


  }

  test("Hello") {
    val config = MushaConfig("/home/oliverr/lunaris/vep/work/h2/egg", "egg", "armeritter")
    val musha = new Musha(config)
    runShowTables(musha)
    val table = Sql.table("Statuses")
    val column = Sql.column("jobId", SqlType.Varchar(20)).asPrimaryKey
    runCreateTable(musha, table, column)
    runShowTables(musha)
    val columnValue =  column.withValue("")
    runInsert(musha, table)
    musha.close()
  }
}
