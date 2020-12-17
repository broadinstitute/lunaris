package lunaris.streams.transform

import akka.stream.scaladsl.{Keep, Source}
import lunaris.recipes.eval.SnagTracker
import lunaris.recipes.values.RecordStreamWithMeta
import lunaris.streams.utils.RecordStreamTypes.Record

class RareMetalsGroupSerializer(override val groupIdFields: Seq[String]) extends GroupSerializer {
  private val rareMetalsHeaderLine: String = "CHROM\tPOS\tREF\tALT\tRS\tAF\tID"

  protected def getString(record: Record, field: String, default: String): String = {
    val stringOpt = for {
      value <- record.values.get(field)
      string <- value.asString.toOption
    } yield string
    stringOpt.getOrElse(default)
  }

  private def recordToDataLine(record: Record, snagTracker: SnagTracker): Seq[String] = {
    getGroupId(record) match {
      case Left(snag) =>
        snagTracker.trackSnag(snag)
        Seq.empty
      case Right(groupId) =>
        val chrom = record.locus.chrom
        val pos = record.locus.region.begin
        val ref = getString(record, "REF", "?")
        val alt = getString(record, "ALT", "?")
        val rs = getString(record, "ID", record.id)
        val af = getString(record, "MAF", "NA")
        val line = Seq(chrom, pos, ref, alt, rs, af, groupId).mkString("\t")
        Seq(line)
    }
  }

  override def recordsToLines(records: RecordStreamWithMeta, snagTracker: SnagTracker):
  Source[String, RecordStreamWithMeta.Meta] = {
    Source.single(rareMetalsHeaderLine).concatMat(records.source
      .mapConcat(recordToDataLine(_, snagTracker)))(Keep.right)
  }
}

object RareMetalsGroupSerializer {
  object Factory extends GroupSerializer.Factory {
    override val name: String = "rareMETALS"
    override def create(groupIdFields: Seq[String]): RareMetalsGroupSerializer =
      new RareMetalsGroupSerializer(groupIdFields)
  }
}
