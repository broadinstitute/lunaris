package musha

import musha.MushaMeta.MetaData
import musha.sql.Sql

import java.sql.{ResultSet, Statement}

sealed trait MushaQuery[A] extends (Statement => A) {
  def sql: Sql
}

object MushaQuery {

  sealed trait WithResultSet[A] extends MushaQuery[A] {
    override def sql: Sql.SqlQuery
  }

  def rowsIter[A](sql: Sql.SqlQuery)(rowMapper: ResultSet => A) = new RowMapping[A](sql)(rowMapper)

  class RowMapping[A](override val sql: Sql.SqlQuery)(rowMapper: ResultSet => A)
    extends WithResultSet[MushaIterator.MapResults[A]] {
    override def apply(statement: Statement): MushaIterator.MapResults[A] =
    {
      val resultSet = statement.executeQuery(sql.sqlString)
      new MushaIterator.MapResults[A](resultSet)(rowMapper)
    }
  }

  def meta[M](sql: Sql.SqlQuery): MetaMapping = new MetaMapping(sql)

  class MetaMapping(override val sql: Sql.SqlQuery) extends WithResultSet[MetaData] {
    override def apply(statement: Statement): MetaData = {
      val resultSet = statement.executeQuery(sql.sqlString)
      MetaData.fromJava(resultSet.getMetaData)
    }
  }

  def metaRowsIter[A](sql: Sql.SqlQuery)(metaRowMapper: (MetaData, ResultSet) => A) =
    new MetaRowMapping[A](sql)(metaRowMapper)

  class MetaRowMapping[A](override val sql: Sql.SqlQuery)(rowMapper: (MetaData, ResultSet) => A)
    extends WithResultSet[MushaIterator.MetaMapResults[A]] {
    override def apply(statement: Statement): MushaIterator.MetaMapResults[A] =
    {
      val resultSet = statement.executeQuery(sql.sqlString)
      new MushaIterator.MetaMapResults[A](resultSet)(rowMapper)
    }
  }

  def update(sql: Sql.SqlNoCount): UpdateWithoutCount = new UpdateWithoutCount(sql)

  class UpdateWithoutCount(override val sql: Sql.SqlNoCount) extends MushaQuery[Unit] {
    override def apply(statement: Statement): Unit = {
      statement.executeUpdate(sql.sqlString)
    }
  }
}
