package lunaris.streams

import lunaris.recipes.values.LunType
import org.broadinstitute.yootilz.core.snag.Snag

case class Header(colNames: Seq[String], seqCol: Int, beginCol: Int, endCol: Int) {
  def asString: String = "#" + colNames.mkString("\t")

  private def pickHeaderField(colNames: Seq[String], name: String, col: Int): Either[Snag, String] = {
    if(col < colNames.length) {
      Right(colNames(col))
    } else {
      Left(Snag(s"Index out of bounds: need to read $name at pos $col, but only have ${colNames.length} elements."))
    }
  }

  def toLunObjectType(idField: String): Either[Snag, LunType.ObjectType] = {
    val snagOrSpecialFields = for {
      chromField <- pickHeaderField(colNames, "chrom", seqCol)
      beginField <- pickHeaderField(colNames, "begin", beginCol)
      endField <- pickHeaderField(colNames, "end", endCol)
    } yield LunType.ObjectSpecialFields(idField, chromField, beginField, endField)
    snagOrSpecialFields.map { specialFields =>
      val elementTypes =
        colNames.map(colName => (colName, LunType.StringType)).toMap +
          (specialFields.begin -> LunType.IntType) + (specialFields.end -> LunType.IntType)
      LunType.ObjectType(specialFields, colNames, elementTypes)
    }
  }
}

object Header {
  def parse(line: String, seqCol: Int, beginCol: Int, endCol: Int): Either[Snag, Header] = {
    var line2: String = line.trim
    while(line2.startsWith("#")) {
      line2 = line2.drop(1)
    }
    val colNames = line2.split("\t").toSeq
    val nColNames = colNames.size
    if(colNames.size < seqCol) {
      Left(Snag(s"Sequence/chromosome column should be column $seqCol, but we only have $nColNames."))
    } else if(colNames.size < beginCol) {
      Left(Snag(s"Begin column should be column $beginCol, but we only have $nColNames."))
    } else if(colNames.size < endCol) {
      Left(Snag(s"End column should be column $endCol, but we only have $nColNames."))
    } else {
      Right(Header(colNames, seqCol, beginCol, endCol))
    }
  }
}
