package lunaris.vep

import akka.stream.Materializer
import akka.stream.scaladsl.Source
import better.files.File
import lunaris.io.{FileInputId, InputId, ResourceConfig}
import lunaris.recipes.values.{LunType, LunValue, RecordStreamWithMeta}
import lunaris.streams.transform.HeaderRecordsParser
import lunaris.streams.utils.RecordStreamTypes.{Record, RecordSource}
import lunaris.utils.NumberParser
import org.broadinstitute.yootilz.core.snag.Snag

object VepOutputReader {

  object ColNames {
    val id = "Uploaded_variation"
    val chrom = "Chrom"
    val pos = "Pos"
    val ref = "Ref"
    val alt = "Alt"
    val pick = "PICK"
  }

  private case class ColIndices(iId: Int, iChrom: Int, iPos: Int, iRef: Int, iAlt: Int)

  private object ColIndices {
    private def indexOf(header: String, headers: Seq[String]): Either[Snag, Int] = {
      val i = headers.indexOf(header)
      if (i >= 0) {
        Right(i)
      } else {
        Left(Snag(s"Missing column $header."))
      }
    }

    def fromHeaders(headers: Seq[String]): Either[Snag, ColIndices] = {
      for {
        iId <- indexOf(ColNames.id, headers)
        iChrom <- indexOf(ColNames.chrom, headers)
        iPos <- indexOf(ColNames.pos, headers)
        iRef <- indexOf(ColNames.ref, headers)
        iAlt <- indexOf(ColNames.alt, headers)
      } yield ColIndices(iId, iChrom, iPos, iRef, iAlt)
    }
  }

  private def pickNonEmptyValue(i: Int, values: Seq[String]): Either[Snag, String] = {
    if (values.length <= i) {
      Left(Snag(s"Need column $i, but only have ${values.length} columns"))
    } else {
      val value = values(i)
      if (value.isEmpty) {
        Left(Snag(s"Value at column $i is empty."))
      } else {
        Right(value)
      }
    }
  }

  private def regionLength(ref: String): Int = if (ref == "-") 0 else ref.length

  private def parseCoreRecord(indices: ColIndices, values: Seq[String]): Either[Snag, Record] = {
    for {
      id <- pickNonEmptyValue(indices.iId, values)
      chrom <- pickNonEmptyValue(indices.iChrom, values)
      posString <- pickNonEmptyValue(indices.iPos, values)
      pos <- NumberParser.parseInt(posString)
      ref <- pickNonEmptyValue(indices.iRef, values)
    } yield LunValue.RecordValue(ColNames.id, id, ColNames.chrom, chrom, ColNames.pos, pos, regionLength(ref))
  }

  private class RecordPicker(snagLogger: Snag => ()) extends (Option[Record] => Seq[Record]) {
    var recordOptStored: Option[Record] = None
    override def apply(recordOpt: Option[Record]): Seq[Record] = {
      (recordOpt, recordOptStored) match {
        case (Some(record), Some(recordStored)) =>
          if(record.id == recordStored.id) {
            record.get(ColNames.pick) match {
              case Left(snag) => snagLogger(snag)
              case Right(pick) =>
              if (pick.toString == "1") {
                recordOptStored = Some(record)
              }
            }
            Seq.empty
          } else {
            recordOptStored = Some(record)
            Seq(recordStored)
          }
        case (Some(record), None) =>
          recordOptStored = Some(record)
          Seq.empty
        case (None, Some(recordStored)) =>
          recordOptStored = None
          Seq(recordStored)
        case (None, None) =>
          Seq.empty
      }
    }
  }

  def read(inputId: InputId, resourceConfig: ResourceConfig, chroms: Seq[String], snagLogger: Snag => ())
          (implicit materializer: Materializer):
  RecordStreamWithMeta = {
    val recordTypeCore = LunType.RecordType(ColNames.id, ColNames.chrom, ColNames.pos, ColNames.pos)
    val recordsWithMeta = HeaderRecordsParser
      .newRecordSource(inputId, resourceConfig, chroms)(recordTypeCore) { headers =>
        ColIndices.fromHeaders(headers)
      } { (recordType, index, values) =>
        parseCoreRecord(index, values).map { coreRecord =>
          var recordTmp = coreRecord
          for (i <- 0 until Math.min(recordType.fields.length, values.length)) {
            val lunValue = LunValue.PrimitiveValue.StringValue(values(i))
            recordTmp = recordTmp.addFieldIFNotExists(recordType.fields(i), lunValue, LunType.StringType)
          }
          recordTmp
        }
      } {
        snagLogger
      }
    val meta = recordsWithMeta.meta
    val records = recordsWithMeta.source
    val recordOpts = records.map(Some(_)) ++ Source.single(None)
    val recordPickerGenerator: () => RecordPicker = () => new RecordPicker(snagLogger)
    val recordsPicked = recordOpts.statefulMapConcat(recordPickerGenerator)
    RecordStreamWithMeta(meta, recordsPicked)
  }
}
