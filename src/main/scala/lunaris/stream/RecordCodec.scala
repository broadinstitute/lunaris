package lunaris.stream

import htsjdk.tribble.AsciiFeatureCodec
import htsjdk.tribble.readers.LineIterator
import lunaris.genomics.{Chromosome, Locus}

case class RecordCodec(chromCol: Int, posCol: Int) extends AsciiFeatureCodec[Record](classOf[Record]){

  def isHeader(line: String): Boolean = line.startsWith("#")

  override def canDecode(path: String): Boolean = path.endsWith("tsv.gz")

  override def readActualHeader(reader: LineIterator): Header = {
    reader.hasNext
    val line = reader.peek()
    println("header")
    println(line)
    val nextLine = reader.next()
    println("header next line")
    println(nextLine)
    Thread.dumpStack()
    if(line != null && isHeader(line)) {
      val headerLine = line.substring(1).trim()
      val colNames = headerLine.split("\t")
      Header(colNames, chromCol, posCol)
    } else {
      null
    }
  }

  override def decode(line: String): Record = {
//    println(line.map(_.toHexString).mkString(" "))
    println("decode")
    println(line)
    Thread.dumpStack()
    if(isHeader(line)) {
      null
    } else {
      val values = line.trim.split("\t")
      val chromosome = Chromosome.parse(values(chromCol)).get
      val pos = values(posCol).toLong
      val locus = Locus(chromosome, pos)
      Record(locus, values)
    }
  }
}
