package musha.sql

sealed trait SqlType[+A] extends SqlElement {

}

object SqlType {

  object SqlInt extends SqlType[Int] {
    override def sqlString: String = "INT"
  }

  object BigInt extends SqlType[Long] {
    override def sqlString: String = "BIGINT"
  }

  case class Varchar(size: Int) extends SqlType[String] {
    override def sqlString: String = s"VARCHAR($size)"
  }

  object Clob extends SqlType[String] {
    override def sqlString: String = "CLOB"
  }
}


