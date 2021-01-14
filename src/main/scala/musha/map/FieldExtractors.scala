package musha.map

import java.sql.ResultSet

object FieldExtractors {

  def str(i: Int): StringByIndex = StringByIndex(i)

  def str(name: String): StringByName = StringByName(name)

  case class StringByIndex(i: Int) extends (ResultSet => String) {
    override def apply(rs: ResultSet): String = rs.getString(i)
  }

  case class StringByName(name: String) extends (ResultSet => String) {
    override def apply(rs: ResultSet): String = rs.getString(name)
  }

}
