package lunaris.vep.vcf

import akka.actor.ActorSystem
import akka.stream.IOResult
import akka.stream.scaladsl.{FileIO, Framing, Sink, Source}
import akka.util.ByteString
import better.files.File
import lunaris.genomics.utils.RegionConsolidator
import lunaris.genomics.{Locus, Region, Variant}
import lunaris.utils.{AkkaUtils, NumberParser}

import scala.collection.mutable
import scala.concurrent.{ExecutionContextExecutor, Future}

object VcfStreamVariantsReader {
  def readVcfRecords(inputFile: File): Source[VcfCore.VcfCoreRecord, Future[IOResult]] = {
    readVcfRecords(FileIO.fromPath(inputFile.path))
  }

  def getValueOrEmptyString(fields: Array[String], i: Int): String = {
    if (i < fields.length) {
      fields(i)
    } else {
      ""
    }
  }

  def readVcfRecords(source: Source[ByteString, Future[IOResult]]):
  Source[VcfCore.VcfCoreRecord, Future[IOResult]] = {
    source.via(Framing.delimiter(ByteString("\n"), Int.MaxValue, allowTruncation = true))
      .map(_.utf8String)
      .filter(!_.startsWith("#"))
      .mapConcat { line =>
        val fields = line.split("\t")
        if (fields.length >= 5) {
          NumberParser.parseInt(fields(VcfCore.ICols.pos)) match {
            case Left(_) => Seq.empty
            case Right(pos) =>
              val chrom = fields(VcfCore.ICols.chrom)
              val id = fields(VcfCore.ICols.id)
              val ref = fields(VcfCore.ICols.ref)
              val alt = fields(VcfCore.ICols.alt)
              val qual = getValueOrEmptyString(fields, VcfCore.ICols.qual)
              val filter = getValueOrEmptyString(fields, VcfCore.ICols.filter)
              val info = getValueOrEmptyString(fields, VcfCore.ICols.info)
              val format = getValueOrEmptyString(fields, VcfCore.ICols.format)
              Seq(VcfCore.VcfCoreRecord(chrom, pos, id, ref, alt, qual, filter, info, format))
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
    implicit val executionContext: ExecutionContextExecutor = AkkaUtils.getDispatcher(actorSystem)
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
    implicit val executionContext: ExecutionContextExecutor = AkkaUtils.getDispatcher(actorSystem)
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
