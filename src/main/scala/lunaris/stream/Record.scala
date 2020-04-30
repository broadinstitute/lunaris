package lunaris.stream

import org.broadinstitute.yootilz.core.snag.Snag

case class Record(header: Header, seq: String, begin: Int, end: Int, values: Seq[String]) {
}

object Record {
  def parse(line: String, header: Header): Either[Snag, Record] = {
    val values = line.trim.split("\t").toSeq
    ???  // TODO
  }
}


