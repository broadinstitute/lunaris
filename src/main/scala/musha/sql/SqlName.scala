package musha.sql

sealed trait SqlName extends SqlElement {
  def name: String
}

object SqlName {

  sealed trait Schema extends SqlName

  sealed trait Table extends SqlName

  sealed trait Column extends SqlName {
    def withType(sqlType: SqlType): SqlColDef = SqlColDef(this, sqlType)
  }

  sealed trait BareName extends SqlName {
    override def sqlString: String = name
  }

  case class BareSchema(name: String) extends Schema with BareName

  case class BareTable(name: String) extends Table with BareName

  case class BareColumn(name: String) extends Column with BareName

  sealed trait QualifiedName[P <: SqlName] extends SqlName {
    def parent: P

    def name: String

    override def sqlString: String = parent.sqlString + "." + name
  }

  case class SchemaTable(parent: Schema, name: String) extends Table with QualifiedName[Schema]

  case class TableColumn(parent: Table, name: String) extends Column with QualifiedName[Table]

}
