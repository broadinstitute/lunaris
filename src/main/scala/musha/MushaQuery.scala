package musha

import java.sql.{ResultSet, ResultSetMetaData, Statement}

sealed trait MushaQuery[A] extends (Statement => A) {
  def sql: Sql
}

object MushaQuery {

  sealed trait WithResultSet[A] extends MushaQuery[A] {
    override def sql: Sql.GivesResultSet
  }

  def apply[A](sql: Sql.GivesResultSet)(rowMapper: ResultSet => A) = new RowMapping[A](sql)(rowMapper)

  class RowMapping[A](override val sql: Sql.GivesResultSet)(rowMapper: ResultSet => A)
    extends WithResultSet[MushaIterator.MapResults[A]] {
    override def apply(statement: Statement): MushaIterator.MapResults[A] =
    {
      val resultSet = statement.executeQuery(sql.sqlString)
      new MushaIterator.MapResults[A](resultSet)(rowMapper)
    }
  }

  def apply[M](sql: Sql.GivesResultSet)(metaMapper: ResultSetMetaData => M): MetaMapping[M] =
    new MetaMapping[M](sql)(metaMapper)

  class MetaMapping[M](override val sql: Sql.GivesResultSet)(metaMapper: ResultSetMetaData => M)
    extends WithResultSet[M] {
    override def apply(statement: Statement): M =
    {
      val resultSet = statement.executeQuery(sql.sqlString)
      metaMapper(resultSet.getMetaData)
    }
  }

  def apply[M, A](sql: Sql.GivesResultSet)(metaMapper: ResultSetMetaData => M)(rowMapper: (M, ResultSet) => A) =
    new MetaRowMapping[M, A](sql)(metaMapper)(rowMapper)

  class MetaRowMapping[M, A](override val sql: Sql.GivesResultSet)(metaMapper: ResultSetMetaData => M)(
    rowMapper: (M, ResultSet) => A
  )
    extends WithResultSet[MushaIterator.MetaMapResults[M, A]] {
    override def apply(statement: Statement): MushaIterator.MetaMapResults[M, A] =
    {
      val resultSet = statement.executeQuery(sql.sqlString)
      new MushaIterator.MetaMapResults[M, A](resultSet)(metaMapper)(rowMapper)
    }
  }
}
