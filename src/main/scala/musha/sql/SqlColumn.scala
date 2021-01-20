package musha.sql

import musha.sql.SqlColumn.Flag

case class SqlColumn[A](name: String, sqlType: SqlType[A], flags: Seq[Flag] = Seq.empty) extends SqlElement {
  def withValue(value: A): SqlColumnValue[A] = SqlColumnValue(this, value)

  def asPrimaryKey: SqlColumn[A] = {
    if(flags.contains(Flag.PrimaryKey)) { this } else copy(flags = flags :+ Flag.PrimaryKey)
  }

  override def sqlString: String = (Seq(name, sqlType.sqlString) ++ flags.map(_.string)).mkString(" ")
}

object SqlColumn {
  case class Flag(string: String)
  object Flag {
    object PrimaryKey extends Flag("PRIMARY KEY")
  }

  def intColumn(name: String): SqlColumn[Int] = SqlColumn(name, SqlType.SqlInt)
}
