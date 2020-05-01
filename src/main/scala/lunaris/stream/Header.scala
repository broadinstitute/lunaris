package lunaris.stream

import org.broadinstitute.yootilz.core.snag.Snag

case class Header(colNames: Seq[String], seqCol: Int, beginCol: Int, endCol: Int) {
  def asString: String = "#" + colNames.mkString("\t")
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
