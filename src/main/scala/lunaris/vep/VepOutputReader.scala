package lunaris.vep

import akka.stream.Materializer
import akka.stream.scaladsl.Source
import lunaris.genomics.Locus
import lunaris.io.{InputId, ResourceConfig}
import lunaris.recipes.values.{LunType, LunValue, RecordStreamWithMeta}
import lunaris.streams.transform.HeaderRecordsParser
import lunaris.streams.utils.RecordStreamTypes.Record
import lunaris.utils.NumberParser
import org.broadinstitute.yootilz.core.snag.Snag

object VepOutputReader {

  object CoreFields {
    val id = "Uploaded_variation"
    val chrom = "Chrom"
    val begin = "Begin"
    val end = "End"
  }

  val recordCoreType: LunType.RecordType =
    LunType.RecordType(CoreFields.id, CoreFields.chrom, CoreFields.begin, CoreFields.end)

  trait FileColNames {
    def id: String
  }

  object FileColNames1 extends FileColNames {
    override val id: String = CoreFields.id
    val location: String = "Location"
  }

  object FileColNames2 extends FileColNames {
    override val id: String = CoreFields.id
    val chrom: String = CoreFields.chrom
    val pos = "Pos"
    val ref = "Ref"
    val alt = "Alt"
    val pick = "PICK"
  }

  trait ColIndices {
    def iId: Int
  }

  private case class ColIndices1(iId: Int, iLocation: Int) extends ColIndices

  private case class ColIndices2(iId: Int, iChrom: Int, iPos: Int, iRef: Int, iAlt: Int) extends ColIndices

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
      if(headers.contains(FileColNames1.location)) {
        for {
          iId <- indexOf(FileColNames1.id, headers)
          iLocation <- indexOf(FileColNames1.location, headers)
        } yield ColIndices1(iId, iLocation)
      } else {
        for {
          iId <- indexOf(FileColNames2.id, headers)
          iChrom <- indexOf(FileColNames2.chrom, headers)
          iPos <- indexOf(FileColNames2.pos, headers)
          iRef <- indexOf(FileColNames2.ref, headers)
          iAlt <- indexOf(FileColNames2.alt, headers)
        } yield ColIndices2(iId, iChrom, iPos, iRef, iAlt)
      }
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
    indices match {
      case ColIndices1(iId, iLocation) =>
        for {
          id <- pickNonEmptyValue(iId, values)
          location <- pickNonEmptyValue(iLocation, values)
          locus <- Locus.parse(location, allowNoEnd = true)
        } yield LunValue.RecordValue(CoreFields.id, id, CoreFields.chrom, locus.chrom, CoreFields.begin,
          locus.region.begin, CoreFields.end, locus.region.end)
      case ColIndices2(iId, iChrom, iPos, iRef, _) =>
        for {
          id <- pickNonEmptyValue(iId, values)
          chrom <- pickNonEmptyValue(iChrom, values)
          posString <- pickNonEmptyValue(iPos, values)
          pos <- NumberParser.parseInt(posString)
          ref <- pickNonEmptyValue(iRef, values)
        } yield LunValue.RecordValue(FileColNames2.id, id, FileColNames2.chrom, chrom, FileColNames2.pos, pos, regionLength(ref))
    }
  }

  private class RecordPicker(snagLogger: Snag => ()) extends (Option[Record] => Seq[Record]) {
    var recordOptStored: Option[Record] = None
    override def apply(recordOpt: Option[Record]): Seq[Record] = {
      (recordOpt, recordOptStored) match {
        case (Some(record), Some(recordStored)) =>
          if(record.id == recordStored.id) {
            record.get(FileColNames2.pick) match {
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
    val recordTypeCore = LunType.RecordType(FileColNames2.id, FileColNames2.chrom, FileColNames2.pos, FileColNames2.pos)
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
