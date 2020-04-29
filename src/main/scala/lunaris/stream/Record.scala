package lunaris.stream

case class Record(header: Header, seq: String, begin: Int, end: Int, values: Seq[String]) {
}


