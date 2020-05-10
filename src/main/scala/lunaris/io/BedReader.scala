package lunaris.io

import lunaris.genomics.Region
import lunaris.io
import lunaris.streams.{Header, Record, RecordProcessor}
import org.broadinstitute.yootilz.core.snag.Snag

import scala.collection.mutable

object BedReader {

  def read(id: InputId, recordProcessor: RecordProcessor, resourceConfig: ResourceConfig = ResourceConfig.empty):
  Either[Snag, Map[String, Seq[Region]]] = {
    id.newReadChannelDisposable(resourceConfig).useUp { readChannel =>
      val bufferSize = 10000
      val refiller = new io.ByteBufferRefiller.FromChannel(readChannel, bufferSize)
      val reader = ByteBufferReader(refiller)
      val snagOrHeader = for {
        line <- reader.readLine()
        header <- Header.parse(line, 1, 2, 3)
      } yield header
      snagOrHeader match {
        case Left(snag) => Left(snag)
        case Right(header) =>
          var regionsBySeq: Map[String, mutable.Builder[Region, Seq[Region]]] = Map.empty
          Record.newEitherator(reader, header, recordProcessor).foreach { record =>
            val regionsForSeq = regionsBySeq.getOrElse(record.seq, Seq.newBuilder[Region])
            regionsForSeq += record.region
            regionsBySeq = regionsBySeq + (record.seq -> regionsForSeq)
          } match {
            case Left(snag) => Left(snag)
            case Right(_) => Right(regionsBySeq.view.mapValues(_.result()).toMap)
          }
      }
    }
  }

}
