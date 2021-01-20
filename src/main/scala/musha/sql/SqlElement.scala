package musha.sql

trait SqlElement {
  def sqlString: String
}

object SqlElement {
  def asSqlLiteral(value: Any): String = {
    value match {
      case number: Int => number.toString
      case number: Long => number.toString
      case number: Short => number.toString
      case number: Char => number.toString
      case number: Byte => number.toString
      case number: Double => number.toString
      case number: Float => number.toString
      case boolean: Boolean => boolean.toString.toUpperCase
      case string: String => "'" + string + "'"
      case _ => "'" + value.toString + "'"
    }
  }
}
