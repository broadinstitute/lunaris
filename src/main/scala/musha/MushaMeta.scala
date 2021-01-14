package musha

import java.sql.{ResultSetMetaData => JMetaData}

object MushaMeta {

  case class Column(i: Int, name: String, label: String, displaySize: Int, typeCode: Int, className: String,
                    precision: Int, scale: Int)

  object Column {
    def fromJava(javaMetaData: JMetaData, i: Int): Column = {
      Column(i, javaMetaData.getColumnName(i), javaMetaData.getColumnLabel(i), javaMetaData.getColumnDisplaySize(i),
        javaMetaData.getColumnType(i), javaMetaData.getColumnClassName(i), javaMetaData.getPrecision(i),
        javaMetaData.getScale(i))
    }
  }

  case class MetaData(columnNames: Seq[String], columns: Map[String, Column])

  object MetaData {
    def fromJava(javaMetaData: JMetaData): MetaData = {
      val nCols = javaMetaData.getColumnCount
      val columnNamesBuilder = Seq.newBuilder[String]
      val columnsBuilder = Map.newBuilder[String, Column]
      for(iCol <- 1 to nCols) {
        val column = Column.fromJava(javaMetaData, iCol)
        val name = column.name
        columnNamesBuilder += name
        columnsBuilder += (name -> column)
      }
      MetaData(columnNamesBuilder.result(), columnsBuilder.result())
    }
  }
}
