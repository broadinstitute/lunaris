package musha.sql

sealed trait SqlType[+A] extends SqlElement {

}

object SqlType {
  object SqlInt extends SqlType[Int] {
    override def sqlString: String = "int"
  }
  case class Varchar(size: Int) extends SqlType[String] {
    override def sqlString: String = s"varchar($size)"
  }
}


