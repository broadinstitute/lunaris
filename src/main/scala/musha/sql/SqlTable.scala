package musha.sql

case class SqlTable(name: String, columns: Seq[SqlColumn[_]])
