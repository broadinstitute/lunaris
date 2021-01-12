package musha

import java.sql.{ResultSet, ResultSetMetaData}

object MushaDefaultMappers {

  val metaMapper: ResultSetMetaData => Seq[MushaMeta.Column] = { metaData =>
    val nCols = metaData.getColumnCount
    val columnsBuilder = Seq.newBuilder[MushaMeta.Column]
    for(iCol <- 1 to nCols) {
      columnsBuilder += MushaMeta.Column.fromJava(metaData, iCol)
    }
    columnsBuilder.result()
  }

  val rowMapper: ResultSet => AnyRef = { resultSet =>
    val metaData = resultSet.getMetaData
    metaRowMapper(metaData, resultSet)
  }

  val metaRowMapper: (ResultSetMetaData, ResultSet) => Map[String, AnyRef] = { (metaData, resultSet) =>
    val columnMetas = metaMapper(metaData)
    val mapBuilder = Map.newBuilder[String, AnyRef]
    for(columnMeta <- columnMetas) {
      val i = columnMeta.i
      val key = columnMeta.name
      val value = resultSet.getObject(i)
      mapBuilder += key -> value
    }
    mapBuilder.result()
  }

  def consumer[A]: Iterator[A] => Seq[A] = iter => iter.toSeq
}
