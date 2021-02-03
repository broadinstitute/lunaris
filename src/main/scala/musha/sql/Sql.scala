package musha.sql

import musha.SqlCodec

import java.sql.Statement

sealed trait Sql extends SqlElement {
}

object Sql {

  sealed trait SqlStatementFunction[A] extends Sql with (Statement => A) {
    override def apply(statement: Statement): A
  }

  private def build(strings: String*): String = strings.filter(_.nonEmpty).mkString(" ") + ";"

  sealed trait SqlQuery extends Sql

  object ShowTables extends SqlQuery {
    override def sqlString: String = "SHOW TABLES;"
  }

  sealed trait SqlDdl extends Sql

  sealed trait SqlNoCount extends SqlDdl

  sealed trait SqlDmlCount extends Sql

  case class CreateTable(table: SqlTable, ifNotExists: Boolean) extends SqlNoCount {
    override def sqlString: String = {
      val colDefsString = table.columns.map(_.sqlString).mkString("(\n  ", ",\n  ", "\n)")
      val ifNotExistsString = if (ifNotExists) "IF NOT EXISTS" else ""
      build("CREATE TABLE", ifNotExistsString, table.name, colDefsString)
    }
  }

  case class DropTable(table: SqlTable, ifExists: Boolean) extends SqlNoCount {
    override def sqlString: String = {
      val ifExistsString = if (ifExists) "IF EXISTS" else ""
      build("DROP TABLE", ifExistsString, table.name)
    }
  }

  case class Insert(table: SqlTable, values: Seq[SqlColumnValue[_]]) extends SqlDmlCount {
    override def sqlString: String = {
      "INSERT INTO " + table.name + "(" +
        values.map(_.sqlColumn.name).mkString("\n  ", ",\n  ", "\n") +
        ") VALUES (" +
        values.map(_.value).map(SqlElement.asSqlLiteral).mkString("\n  ", ",\n  ", "\n") +
        ")"
    }
  }

  case class Merge(table: SqlTable, values: Seq[SqlColumnValue[_]]) extends SqlDmlCount {
    override def sqlString: String = {
      "MERGE INTO " + table.name + "(" +
        values.map(_.sqlColumn.name).mkString("\n  ", ",\n  ", "\n") +
        ") VALUES (" +
        values.map(_.value).map(SqlElement.asSqlLiteral).mkString("\n  ", ",\n  ", "\n") +
        ")"
    }
  }

  case class Select(table: SqlTable) extends SqlQuery {
    override def sqlString: String = s"SELECT * FROM ${table.name};"
  }

  sealed trait Filter extends SqlElement

  case class Equals[A](column: SqlColumn[A], value: A) extends Filter {
    override def sqlString: String = column.name + "=" + SqlElement.asSqlLiteral(value)
  }

  case class SelectWhere(table: SqlTable, filter: Filter) extends SqlQuery {
    override def sqlString: String = s"SELECT * FROM ${table.name} WHERE ${filter.sqlString};"
  }

  case class Delete(table: SqlTable, filter: Filter) extends SqlDmlCount {
    override def sqlString: String = s"DELETE FROM ${table.name} WHERE ${filter.sqlString};"
  }

  case class SelectCountRows(table: SqlTable) extends SqlStatementFunction[Long] {
    override def sqlString: String = s"SELECT COUNT(*) FROM ${table.name};"

    override def apply(statement: Statement): Long = {
      val resultSet = statement.executeQuery(sqlString)
      resultSet.next()
      resultSet.getLong(1)
    }
  }

  def table(name: String, columns: SqlCodec[_, _]*): SqlTable = SqlTable(name, columns.map(_.sqlColumn))

  def column[A](name: String, sqlType: SqlType[A]): SqlColumn[A] = SqlColumn(name, sqlType)

  def showTables: ShowTables.type = ShowTables

  def createTable(table: SqlTable): CreateTable = CreateTable(table, ifNotExists = false)

  def createTableIfNotExists(table: SqlTable): CreateTable = CreateTable(table, ifNotExists = true)

  def dropTable(table: SqlTable): DropTable = DropTable(table, ifExists = false)

  def dropTableIfNotExists(table: SqlTable): DropTable = DropTable(table, ifExists = true)

  def insert(table: SqlTable, columnValues: SqlColumnValue[_]*): Insert = Insert(table, columnValues)

  def merge(table: SqlTable, columnValues: SqlColumnValue[_]*): Merge = Merge(table, columnValues)

  def select(table: SqlTable): Select = Select(table)

  def select(table: SqlTable, filter: Filter): SelectWhere = SelectWhere(table, filter)

  def delete(table: SqlTable, filter: Filter): Delete = Delete(table, filter)

  def countRows(table: SqlTable): SelectCountRows = SelectCountRows(table)
}
