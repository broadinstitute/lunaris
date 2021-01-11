package musha

trait Sql {
  def sqlString: String

}

object Sql {
  trait GivesResultSet extends Sql
  object ShowTables extends GivesResultSet {
    override def sqlString: String = "show tables;"
  }
}
