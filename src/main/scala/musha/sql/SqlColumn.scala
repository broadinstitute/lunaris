package musha.sql

import musha.SqlCodec
import musha.sql.SqlColumn.Flag

import java.sql.ResultSet

case class SqlColumn[A](name: String, sqlType: SqlType[A], flags: Seq[Flag] = Seq.empty)
  extends SqlCodec[A, A] with SqlElement {
  def withValue(value: A): SqlColumnValue[A] = SqlColumnValue(this, value)

  override def asPrimaryKey: SqlColumn[A] = {
    if (flags.contains(Flag.PrimaryKey)) this else copy(flags = flags :+ Flag.PrimaryKey)
  }

  override def sqlString: String = (Seq(name, sqlType.sqlString) ++ flags.map(_.string)).mkString(" ")

  def get: ResultSet => A = sqlType.get(name)

  override def sqlColumn: SqlColumn[A] = this

  override def bimap[C](encode: C => A, decode: A => C): SqlCodec.Mapped[A, C] =
    SqlCodec.Mapped(this, encode, decode)
}

object SqlColumn {

  case class Flag(string: String)

  object Flag {

    object PrimaryKey extends Flag("PRIMARY KEY")

  }

  def intColumn(name: String): SqlColumn[Int] = SqlColumn(name, SqlType.SqlInt)
}
