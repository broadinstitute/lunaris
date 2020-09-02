package lunaris.varianteffect

import akka.actor.ActorSystem
import akka.stream.IOResult
import akka.stream.scaladsl.{FileIO, Framing, Sink, Source}
import akka.util.ByteString
import better.files.File
import lunaris.genomics.utils.RegionConsolidator
import lunaris.genomics.{Locus, Region, Variant}
import lunaris.utils.NumberParser

import scala.collection.mutable
import scala.concurrent.{ExecutionContextExecutor, Future}

object VcfStreamVariantsReader {

  object Cols {
    val chrom: Int = 0
    val pos: Int = 1
    val id: Int = 2
    val ref: Int = 3
    val alt: Int = 4
  }

  case class VcfCoreRecord(chrom: String, pos: Int, id: String, ref: String, alt: String) {
    def toVariant: Variant = Variant(chrom, pos, ref, alt)
    def toLocus: Locus = toVariant.toLocus
  }

  def readVariants(inputFile: File): Source[VcfCoreRecord, Future[IOResult]] = {
    FileIO.fromPath(inputFile.path).via(Framing.delimiter(ByteString("\n"), Int.MaxValue))
      .map(_.utf8String)
      .filter(!_.startsWith("#"))
      .mapConcat { line =>
        val fields = line.split("\t")
        if (fields.length >= 5) {
          NumberParser.parseInt(fields(Cols.pos)) match {
            case Left(_) => Seq.empty
            case Right(pos) =>
              val chrom = fields(Cols.chrom)
              val id = fields(Cols.id)
              val ref = fields(Cols.ref)
              val alt = fields(Cols.alt)
              Seq(VcfCoreRecord(chrom, pos, id, ref, alt))
          }
        } else {
          Seq.empty
        }
      }
  }

  def readConsolidatedRegionsByChrom(inputFile: File)(
    implicit actorSystem: ActorSystem
  ): Future[Map[String, Seq[Region]]] = {
    implicit val executionContext: ExecutionContextExecutor = actorSystem.dispatcher
    readVariants(inputFile).map(_.toLocus).map(Some(_)).concat(Source.single(None)).statefulMapConcat { () =>
      val builder = RegionConsolidator.newBuilderForSorted()
      (locusOpt: Option[Locus]) =>
        locusOpt match {
          case Some(locus) =>
            builder.add(locus)
            Seq()
          case None =>
            Seq(builder.result())
      }
    }.runWith(Sink.head)
  }

  def readVariantsByChrom(inputFile: File)(
    implicit actorSystem: ActorSystem
  ): Future[Map[String, Seq[Variant]]] = {
    implicit val executionContext: ExecutionContextExecutor = actorSystem.dispatcher
    readVariants(inputFile).map(_.toVariant)
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
