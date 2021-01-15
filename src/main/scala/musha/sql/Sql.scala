package musha.sql

import musha.sql.SqlName.{BareColumn, BareTable, Table}

sealed trait Sql extends SqlElement {
}

object Sql {

  sealed trait SqlQuery extends Sql

  object ShowTables extends SqlQuery {
    override def sqlString: String = "show tables;"
  }

  sealed trait SqlDdl extends Sql

  sealed trait SqlNoCount extends SqlDdl

  case class CreateTable(table: Table, colDefs: Seq[SqlColDef]) extends SqlNoCount {
    override def sqlString: String = {
      val colDefsString = "\n" + colDefs.map(_.sqlString).mkString("\n  ", "\n  ", "\n")
      s"create table ${table.sqlString} ($colDefsString);"
    }
  }

  def table(name: String): BareTable = BareTable(name)

  def column(name: String): BareColumn = BareColumn(name)

  def showTables: ShowTables.type = ShowTables

  def createTable(table: Table, colDefs: Seq[SqlColDef]): CreateTable = CreateTable(table, colDefs)


}
