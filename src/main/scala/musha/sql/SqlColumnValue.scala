package musha.sql

import musha.SqlCodec

case class SqlColumnValue[A](sqlColumn: SqlColumn[A], value: A)
