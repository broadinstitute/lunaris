package lunaris.stream

import lunaris.genomics.Region
import lunaris.io.{ByteBufferReader, ByteBufferRefiller}
import lunaris.utils.{DebugUtils, Eitherator, NumberParser}
import org.broadinstitute.yootilz.core.snag.Snag

case class Record(header: Header, seq: String, region: Region, values: Seq[String]) {
  def asString: String = values.mkString("\t")
}

object Record {

  private def outOfBoundsSnag(name: String, index: Int, size: Int): Snag =
    Snag(s"Trying to read field $name from column $index, but record only has $size columns.")

  private def pickField(values: Seq[String], name: String, index: Int): Either[Snag, String] = {
    if (values.size > index) {
      Right(values(index))
    } else {
      Left(outOfBoundsSnag(name, index, values.size))
    }
  }

  private def parseField[T](values: Seq[String], name: String,
                            index: Int)(parser: String => Either[Snag, T]): Either[Snag, T] = {
    for {
      _ <-
        if (values.size > index) {
          Right(())
        } else {
          Left(outOfBoundsSnag(name, index, values.size))
        }
      value <- parser(values(index))
    } yield value
  }

  def parse(line: String, header: Header): Either[Snag, Record] = {
    DebugUtils.println(line)
    val values = line.trim.split("\t").toSeq
    DebugUtils.println(values)
    for {
      seq <- pickField(values, "sequence", header.seqCol - 1)
      begin <- parseField(values, "begin", header.beginCol - 1)(NumberParser.parseInt)
      end <-
        if(header.beginCol == header.endCol) {
          Right(begin + 1)  // at least, tabix assumes that for indexing
        } else {
          parseField(values, "end", header.endCol - 1)(NumberParser.parseInt)
        }
    } yield Record(header, seq, Region(begin, end), values)
  }

  def newEitherator(reader: ByteBufferReader,
                    header: Header,
                    recordProcessor: RecordProcessor): Eitherator[Record] = {
    val lineEitherator =
      Eitherator.fromGenerator(!reader.refiller.isExhausted)(reader.readLine())
    lineEitherator.process { line =>
      recordProcessor(Record.parse(line, header))
    }
  }
}


