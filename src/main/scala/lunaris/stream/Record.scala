package lunaris.stream

import lunaris.genomics.Locus

case class Record(locus: Locus, values: Seq[String]) {
}
