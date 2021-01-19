package musha.sql

import musha.sql.SqlName.Column

case class SqlColDef(column: Column) {  //  TODO: remove class
}

object SqlColDef {

  sealed trait Flags extends SqlElement {
  }

  object Flags {

    object None extends Flags {
      override def sqlString: String = ""
    }

    object PrimaryKey extends Flags {
      override def sqlString: String = "PRIMARY KEY"
    }

  }

}
