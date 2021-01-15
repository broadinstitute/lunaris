package musha.sql

import musha.sql.SqlColDef.Flags
import musha.sql.SqlName.Column

case class SqlColDef(column: Column, sqlType: SqlType, flags: Flags = Flags.None) extends SqlElement {
  override def sqlString: String = {
    flags match {
      case Flags.None => column.sqlString + " " + sqlType.sqlString
      case _ => column.sqlString + " " + sqlType.sqlString + " " + flags.sqlString
    }
  }

  def asPrimaryKey: SqlColDef = copy(flags = flags.asPrimaryKey)
}

object SqlColDef {

  sealed trait Flags extends SqlElement {
    def asPrimaryKey: Flags.PrimaryKey.type = Flags.PrimaryKey
  }

  object Flags {

    object None extends Flags {
      override def sqlString: String = ""
    }

    object PrimaryKey extends Flags {
      override def sqlString: String = "primary key"
    }

  }

}
