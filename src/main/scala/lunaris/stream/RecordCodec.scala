package lunaris.stream

import htsjdk.tribble.AsciiFeatureCodec
import htsjdk.tribble.readers.LineIterator
import lunaris.genomics.{Chromosome, Locus}

case class RecordCodec(chromCol: Int, posCol: Int) extends AsciiFeatureCodec[Record](classOf[Record]){
  override def canDecode(path: String): Boolean = path.endsWith("tsv.gz")

  override def readActualHeader(reader: LineIterator): Header = {
    val headerLineRaw = reader.next()
    println(headerLineRaw)
    var headerLineTrimmed: String = headerLineRaw
    if(headerLineTrimmed.startsWith("#")) {
      headerLineTrimmed = headerLineTrimmed.substring(1)
    }
    headerLineTrimmed = headerLineTrimmed.trim
    val colNames = headerLineTrimmed.split("\t")
    Header(colNames, chromCol, posCol)
  }

  override def decode(line: String): Record = {
    println(line.map(_.toHexString).mkString(" "))
//    println(line)
    val values = line.trim.split("\t")
    val chromosome = Chromosome.parse(values(chromCol)).get
    val pos = values(posCol).toLong
    val locus = Locus(chromosome, pos)
    Record(locus, values)
  }
}
