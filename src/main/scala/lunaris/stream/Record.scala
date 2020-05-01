package lunaris.stream

import lunaris.genomics.Region
import lunaris.utils.NumberParser
import org.broadinstitute.yootilz.core.snag.Snag

case class Record(header: Header, seq: String, region: Region, values: Seq[String]) {
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
    val values = line.trim.split("\t").toSeq
    for {
      seq <- pickField(values, "sequence", header.seqCol)
      begin <- parseField(values, "begin", header.beginCol)(NumberParser.parseInt)
      end <-
        if(header.beginCol == header.endCol) {
          Right(begin + 1)  // at least, tabix assumes that for indexing
        } else {
          parseField(values, "end", header.endCol)(NumberParser.parseInt)
        }
    } yield Record(header, seq, Region(begin, end), values)
  }
}


