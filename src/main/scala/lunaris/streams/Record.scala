package lunaris.streams

import lunaris.genomics.{Locus, Region}
import lunaris.io.ByteBufferReader
import lunaris.recipes.values.LunValue
import lunaris.utils.{Eitherator, NumberParser}
import org.broadinstitute.yootilz.core.snag.{Snag, SnagTag}

case class Record(header: Header, locus: Locus, values: Seq[String]) {
  def asString: String = values.mkString("\t")

  def toObject(idField: String): Either[Snag, LunValue.ObjectValue] = {
    header.toLunObjectType(idField).flatMap { objectType =>
      val idCol = header.colNames.indexOf(idField)
      if (idCol < 0) {
        Left(Snag(s"Header line does not contain id field '$idField'."))
      } else if (idCol >= values.size) {
        Left(Snag(s"Missing id: should be in column $idCol, but record has only ${values.size} fields."))
      } else {
        val id = values(idCol)
        val objectValues =
          header.colNames.zip(values.map(LunValue.PrimitiveValue.StringValue)).toMap[String, LunValue] +
            (objectType.specialFields.chrom -> LunValue.PrimitiveValue.StringValue(locus.chrom)) +
            (objectType.specialFields.begin -> LunValue.PrimitiveValue.IntValue(locus.region.begin)) +
            (objectType.specialFields.end -> LunValue.PrimitiveValue.IntValue(locus.region.end))
        Right(LunValue.ObjectValue(id, locus, objectType, objectValues))
      }
    }
  }
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
      seq <- pickField(values, "sequence", header.seqCol - 1)
      begin <- parseField(values, "begin", header.beginCol - 1)(NumberParser.parseInt)
      end <-
        if (header.beginCol == header.endCol) {
          Right(begin + 1) // at least, tabix assumes that for indexing
        } else {
          parseField(values, "end", header.endCol - 1)(NumberParser.parseInt)
        }
    } yield Record(header, Locus(seq, Region(begin, end)), values)
  }

  def newEitherator(reader: ByteBufferReader,
                    header: Header,
                    recordProcessor: RecordProcessor[Record]): Eitherator[Record] = {
    val lineEitherator =
      Eitherator.fromGeneratorUntilTag(SnagTag.endOfData)(reader.readLine())
    lineEitherator.process { line =>
      recordProcessor(Record.parse(line, header))
    }
  }
}


