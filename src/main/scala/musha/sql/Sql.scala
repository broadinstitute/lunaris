package musha.sql

import musha.sql.SqlName.{BareTable, Table}

sealed trait Sql extends SqlElement {
}

object Sql {

  private def build(strings: String*): String = strings.filter(_.nonEmpty).mkString(" ") + ";"

  sealed trait SqlQuery extends Sql

  object ShowTables extends SqlQuery {
    override def sqlString: String = "SHOW TABLES;"
  }

  sealed trait SqlDdl extends Sql

  sealed trait SqlNoCount extends SqlDdl

  sealed trait SqlDmlCount extends Sql

  case class CreateTable(table: Table, ifNotExists: Boolean, columns: Seq[SqlColumn[_]]) extends SqlNoCount {
    override def sqlString: String = {
      val colDefsString = columns.map(_.sqlString).mkString("(\n  ", "\n  ", "\n)")
      val ifNotExistsString = if (ifNotExists) "IF NOT EXISTS" else ""
      build("CREATE TABLE", ifNotExistsString, table.sqlString, colDefsString)
    }
  }

  case class DropTable(table: Table, ifExists: Boolean) extends SqlNoCount {
    override def sqlString: String = {
      val ifExistsString = if (ifExists) "IF EXISTS" else ""
      build("DROP TABLE", ifExistsString, table.sqlString)
    }
  }

  case class Insert(table: Table, values: Seq[SqlColumnValue[_]]) extends SqlDmlCount {
    override def sqlString: String = {
      "INSERT INTO " + table.sqlString + "(" +
        values.map(_.sqlColumn.sqlString).mkString("\n  ", "\n  ", "\n") +
        ") VALUES (" +
        values.map(_.value).map(SqlElement.asSqlLiteral).mkString("\n  ", "\n  ", "\n") +
        ")"
    }
  }

  case class Select(table: Table) extends SqlQuery {
    override def sqlString: String = s"SELECT * FROM ${table.sqlString};"
  }

  def table(name: String): BareTable = BareTable(name)

  def column[A](name: String, sqlType: SqlType[A]): SqlColumn[A] = SqlColumn(name, sqlType)

  def showTables: ShowTables.type = ShowTables

  def createTable(table: Table, columns: Seq[SqlColumn[_]]): CreateTable =
    CreateTable(table, ifNotExists = false, columns)

  def createTableIfNotExists(table: Table, columns: Seq[SqlColumn[_]]): CreateTable =
    CreateTable(table, ifNotExists = true, columns)

  def dropTable(table: Table): DropTable = DropTable(table, ifExists = false)

  def dropTableIfNotExists(table: Table): DropTable = DropTable(table, ifExists = true)

  def insert(table: Table, columnValues: SqlColumnValue[_]*): Insert = Insert(table, columnValues)

  def select(table: Table): Select = Select(table)
}
