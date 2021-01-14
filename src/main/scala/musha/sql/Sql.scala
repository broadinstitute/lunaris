package musha.sql

import musha.sql.SqlName.Table

sealed trait Sql extends SqlElement {
}

object Sql {

  sealed trait SqlQuery extends Sql

  object ShowTables extends SqlQuery {
    override def sqlString: String = "show tables;"
  }

  sealed trait SqlDdl extends Sql

  sealed trait SqlNoCount extends SqlDdl

  case class CreateTable(table: Table) extends SqlNoCount {
    override def sqlString: String = s"create table ${table.sqlString}"  //  TODO column definitions
  }



}
