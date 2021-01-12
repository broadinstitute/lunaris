package musha

import musha.MushaMeta.MetaData

import java.sql.ResultSet

trait MushaIterator[A] extends Iterator[A] {
  protected def resultSet: ResultSet
  val metaData: MetaData = MetaData.fromJava(resultSet.getMetaData)
  var hasNextField: Boolean = resultSet.next()

  override def hasNext: Boolean = hasNextField
}

object MushaIterator {

  class MapResults[A](protected val resultSet: ResultSet)(rowMapper: ResultSet => A) extends MushaIterator[A] {
    override def next(): A = {
      val a = rowMapper(resultSet)
      hasNextField = resultSet.next()
      a
    }
  }

  class MetaMapResults[A](protected val resultSet: ResultSet)(rowMapper: (MetaData, ResultSet) => A)
    extends MushaIterator[A] {

    override def next(): A = {
      val a = rowMapper(metaData, resultSet)
      hasNextField = resultSet.next()
      a
    }
  }

}
