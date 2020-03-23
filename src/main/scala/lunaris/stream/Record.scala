package lunaris.stream

import htsjdk.tribble.Feature
import lunaris.genomics.Locus

case class Record(locus: Locus, header: Header, values: Map[String, String]) extends Feature {
  override def getContig: String = locus.chromosome.asString

  override def getStart: Int = locus.pos.toInt

  override def getEnd: Int = locus.pos.toInt
}
