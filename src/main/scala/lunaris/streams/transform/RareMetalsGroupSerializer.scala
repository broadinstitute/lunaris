package lunaris.streams.transform

import akka.stream.scaladsl.{Keep, Source}
import lunaris.recipes.eval.SnagTracker
import lunaris.recipes.values.RecordStreamWithMeta
import lunaris.streams.utils.RecordStreamTypes.Record
import org.broadinstitute.yootilz.core.snag.Snag

class RareMetalsGroupSerializer(groupIdFields: Seq[String]) {
  private val rareMetalsHeaderLine: String = "CHROM\tPOS\tREF\tALT\tRS\tAF\tID"

  protected def getString(record: Record, field: String, default: String): String = {
    val stringOpt = for {
      value <- record.values.get(field)
      string <- value.asString.toOption
    } yield string
    stringOpt.getOrElse(default)
  }

  protected def getGroupId(record: Record): Either[Snag, String] = {
    var snagOpt: Option[Snag] = None
    var groupIdOpt: Option[String] = None
    val groupIdFieldIter: Iterator[String] = groupIdFields.iterator
    while(groupIdFieldIter.hasNext && snagOpt.isEmpty && groupIdOpt.isEmpty ) {
      val groupIdField = groupIdFieldIter.next()
      if(record.has(groupIdField)) {
        record.get(groupIdField).flatMap(_.asString) match {
          case Left(snag) => snagOpt = Some(snag)
          case Right(groupId) => groupIdOpt = Some(groupId)
        }
      }
    }
    (snagOpt, groupIdOpt) match {
      case (Some(snag), _) => Left(snag)
      case (_, Some(groupId)) => Right(groupId)
      case _ => Left(Snag(s"Record ${record.id} does not have any of the fields ${groupIdFields.mkString(", ")}."))
    }
  }

  private def recordToRareMetalsDataLine(record: Record, snagTracker: SnagTracker): Seq[String] = {
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

  def recordsToRareMetalsLines(recordStream: RecordStreamWithMeta, snagTracker: SnagTracker):
  Source[String, RecordStreamWithMeta.Meta] = {
    Source.single(rareMetalsHeaderLine).concatMat(recordStream.source
      .mapConcat(recordToRareMetalsDataLine(_, snagTracker)))(Keep.right)
  }


}
