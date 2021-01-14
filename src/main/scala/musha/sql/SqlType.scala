package musha.sql

sealed trait SqlType extends SqlElement {

}

object SqlType {
  object SqlInt extends SqlType {
    override def sqlString: String = "int"
  }
  case class Varchar(size: Int) extends SqlType {
    override def sqlString: String = s"varchar($size)"
  }
}


