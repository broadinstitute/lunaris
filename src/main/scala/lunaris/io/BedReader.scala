package lunaris.io

import lunaris.genomics.Region
import lunaris.io
import lunaris.streams.{TsvHeader, TsvRecord, RecordProcessor}
import org.broadinstitute.yootilz.core.snag.Snag

import scala.collection.mutable

object BedReader {

  def read(id: InputId, recordProcessor: RecordProcessor[TsvRecord],
           resourceConfig: ResourceConfig = ResourceConfig.empty):
  Either[Snag, Map[String, Seq[Region]]] = {
    id.newReadChannelDisposable(resourceConfig).useUp { readChannel =>
      val bufferSize = 10000
      val refiller = new io.ByteBufferRefiller.FromChannel(readChannel, bufferSize)
      val reader = ByteBufferReader(refiller)
      val snagOrHeader = for {
        line <- reader.readLine()
        header <- TsvHeader.parseLine(line, 1, 2, 3)
      } yield header
      snagOrHeader match {
        case Left(snag) => Left(snag)
        case Right(header) =>
          var regionsBySeq: Map[String, mutable.Builder[Region, Seq[Region]]] = Map.empty
          TsvRecord.newEitherator(reader, header, recordProcessor).foreach { record =>
            val regionsForSeq = regionsBySeq.getOrElse(record.locus.chrom, Seq.newBuilder[Region])
            regionsForSeq += record.locus.region
            regionsBySeq = regionsBySeq + (record.locus.chrom -> regionsForSeq)
          } match {
            case Left(snag) => Left(snag)
            case Right(_) => Right(regionsBySeq.view.mapValues(_.result()).toMap)
          }
      }
    }
  }

}
