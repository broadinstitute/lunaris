package musha.sql

case class SqlColumnValue[A](sqlColumn: SqlColumn[A], value: A)
