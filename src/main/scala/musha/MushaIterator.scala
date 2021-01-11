package musha

import java.sql.{ResultSet, ResultSetMetaData}

trait MushaIterator[A] extends Iterator[A] {
  protected def resultSet: ResultSet

  override def hasNext: Boolean = !resultSet.isAfterLast
}

object MushaIterator {

  class MapResults[A](protected val resultSet: ResultSet)(rowMapper: ResultSet => A) extends MushaIterator[A] {
    override def next(): A = {
      val a = rowMapper(resultSet)
      resultSet.next()
      a
    }
  }

  class MetaMapResults[M, A](protected val resultSet: ResultSet)(metaMapper: ResultSetMetaData => M)(
    rowMapper: (M, ResultSet) => A
  ) extends MushaIterator[A] {
    val meta: M = metaMapper(resultSet.getMetaData)

    override def next(): A = {
      val a = rowMapper(meta, resultSet)
      resultSet.next()
      a
    }
  }

}
