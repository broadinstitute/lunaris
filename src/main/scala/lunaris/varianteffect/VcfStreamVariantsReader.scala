package lunaris.varianteffect

import akka.actor.ActorSystem
import akka.stream.IOResult
import akka.stream.scaladsl.{FileIO, Framing, Sink, Source}
import akka.util.ByteString
import better.files.File
import lunaris.genomics.utils.RegionConsolidator
import lunaris.genomics.{Locus, Region, Variant}
import lunaris.recipes.values.{LunType, LunValue}
import lunaris.streams.utils.RecordStreamTypes.Record
import lunaris.utils.NumberParser
import lunaris.varianteffect.VcfStreamVariantsReader.ColNames

import scala.collection.mutable
import scala.concurrent.{ExecutionContextExecutor, Future}

object VcfStreamVariantsReader {

  object ICols {
    val chrom: Int = 0
    val pos: Int = 1
    val id: Int = 2
    val ref: Int = 3
    val alt: Int = 4
    val qual: Int = 5
    val filter: Int = 6
    val info: Int = 7
    val format = 8
  }

  object ColNames {
    val chrom = "CHROM"
    val pos = "POS"
    val id = "ID"
    val ref = "REF"
    val alt = "ALT"
    val qual = "QUAL"
    val filter = "FILTER"
    val info = "INFO"
    val format = "FORMAT"
    val seq = Seq(chrom, pos, id, ref, alt, qual, filter, info, format)
  }

  val vcfRecordType: LunType.RecordType =
    LunType.RecordType(ColNames.id, ColNames.chrom, ColNames.pos, ColNames.pos)
      .addField(ColNames.ref, LunType.StringType)
      .addField(ColNames.alt, LunType.StringType)
      .addField(ColNames.qual, LunType.StringType)
      .addField(ColNames.filter, LunType.StringType)
      .addField(ColNames.info, LunType.StringType)
      .addField(ColNames.format, LunType.StringType)

  case class VcfCoreRecord(chrom: String, pos: Int, id: String, ref: String, alt: String, qual: String,
                           filter: String, info: String, format: String) {
    def toVariant: Variant = Variant(chrom, pos, ref, alt)

    def toLocus: Locus = toVariant.toLocus

    def toRecord: Record = {
      val locus = toLocus
      val values = Map(
        ColNames.chrom -> LunValue.PrimitiveValue.StringValue(chrom),
        ColNames.pos -> LunValue.PrimitiveValue.IntValue(pos),
        ColNames.id -> LunValue.PrimitiveValue.StringValue(id),
        ColNames.ref -> LunValue.PrimitiveValue.StringValue(ref),
        ColNames.alt -> LunValue.PrimitiveValue.StringValue(alt),
        ColNames.qual -> LunValue.PrimitiveValue.StringValue(qual),
        ColNames.filter -> LunValue.PrimitiveValue.StringValue(filter),
        ColNames.info -> LunValue.PrimitiveValue.StringValue(info),
        ColNames.format -> LunValue.PrimitiveValue.StringValue(format),
      )
      LunValue.RecordValue(id, locus, vcfRecordType, values)
    }
  }

  def readVcfRecords(inputFile: File): Source[VcfCoreRecord, Future[IOResult]] = {
    readVcfRecords(FileIO.fromPath(inputFile.path))
  }

  def getValueOrEmptyString(fields: Array[String], i: Int): String = {
    if(i < fields.length) {
      fields(i)
    } else {
      ""
    }
  }

  def readVcfRecords(source: Source[ByteString, Future[IOResult]]):
  Source[VcfCoreRecord, Future[IOResult]] = {
    source.via(Framing.delimiter(ByteString("\n"), Int.MaxValue))
      .map(_.utf8String)
      .filter(!_.startsWith("#"))
      .mapConcat { line =>
        val fields = line.split("\t")
        if (fields.length >= 5) {
          NumberParser.parseInt(fields(ICols.pos)) match {
            case Left(_) => Seq.empty
            case Right(pos) =>
              val chrom = fields(ICols.chrom)
              val id = fields(ICols.id)
              val ref = fields(ICols.ref)
              val alt = fields(ICols.alt)
              val qual = getValueOrEmptyString(fields, ICols.qual)
              val filter = getValueOrEmptyString(fields, ICols.filter)
              val info = getValueOrEmptyString(fields, ICols.info)
              val format = getValueOrEmptyString(fields, ICols.format)
              Seq(VcfCoreRecord(chrom, pos, id, ref, alt, qual, filter, info, format))
          }
        } else {
          Seq.empty
        }
      }
  }

  case class ChromsAndRegions(chroms: Seq[String], regions: Map[String, Seq[Region]])

  def readChromsAndRegions(inputFile: File)(
    implicit actorSystem: ActorSystem
  ): Future[ChromsAndRegions] = {
    implicit val executionContext: ExecutionContextExecutor = actorSystem.dispatcher
    readVcfRecords(inputFile).map(_.toLocus).map(Some(_)).concat(Source.single(None)).statefulMapConcat { () =>
      var chroms: Seq[String] = Seq()
      var chromSet: Set[String] = Set()
      val builder = RegionConsolidator.newBuilderForSorted()
      (locusOpt: Option[Locus]) =>
        locusOpt match {
          case Some(locus) =>
            val chrom = locus.chrom
            if (!chromSet(chrom)) {
              chromSet += chrom
              chroms :+= chrom
            }
            builder.add(locus)
            Seq()
          case None =>
            Seq(ChromsAndRegions(chroms, builder.result()))
        }
    }.runWith(Sink.head)
  }

  def readVariantsByChrom(inputFile: File)(
    implicit actorSystem: ActorSystem
  ): Future[Map[String, Seq[Variant]]] = {
    implicit val executionContext: ExecutionContextExecutor = actorSystem.dispatcher
    readVcfRecords(inputFile).map(_.toVariant)
      .runFold(Map.empty[String, mutable.Builder[Variant, Seq[Variant]]]) { (variantsByChrom, variant) =>
        val chrom = variant.chrom
        variantsByChrom.get(chrom) match {
          case Some(variantsForChrom) =>
            variantsForChrom += variant
            variantsByChrom
          case None =>
            val variantsForChrom = Seq.newBuilder[Variant]
            variantsForChrom += variant
            variantsByChrom + (chrom -> variantsForChrom)
        }
      }.map(_.view.mapValues(_.result()).toMap)
  }

}
