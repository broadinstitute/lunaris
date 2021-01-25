package musha.sql

import java.sql.ResultSet

sealed trait SqlType[+A] extends SqlElement {
  def get(name: String): ResultSet => A
}

object SqlType {

  object SqlInt extends SqlType[Int] {
    override def sqlString: String = "INT"

    override def get(name: String): ResultSet => Int = _.getInt(name)
  }

  object BigInt extends SqlType[Long] {
    override def sqlString: String = "BIGINT"

    override def get(name: String): ResultSet => Long = _.getLong(name)
  }

  sealed trait StringType extends SqlType[String] {
    override def get(name: String): ResultSet => String = _.getString(name)
  }

  case class Varchar(size: Int) extends StringType {
    override def sqlString: String = s"VARCHAR($size)"
  }

  object Clob extends StringType {
    override def sqlString: String = "CLOB"
  }

}


