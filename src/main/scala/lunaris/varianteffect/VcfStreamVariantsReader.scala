package lunaris.varianteffect

import java.nio.file.Path

import akka.actor.ActorSystem
import akka.stream.scaladsl.{FileIO, Framing, Source}
import akka.util.ByteString
import better.files.File
import lunaris.genomics.Variant
import lunaris.utils.NumberParser

import scala.collection.mutable
import scala.concurrent.{ExecutionContextExecutor, Future}

object VcfStreamVariantsReader {
  def newVariantsByChromFuture(stream: Source[ByteString, Any], inputFile: File)(
    implicit actorSystem: ActorSystem
  ): Future[Map[String, Seq[Variant]]] = {
    implicit val executionContext: ExecutionContextExecutor = actorSystem.dispatcher

    val fileSaveFut = stream.runWith(FileIO.toPath(inputFile.path))

    fileSaveFut.flatMap { _ =>
      FileIO.fromPath(inputFile.path).via(Framing.delimiter(ByteString("\n"), Int.MaxValue))
        .map(_.utf8String)
        .filter(!_.startsWith("#"))
        .mapConcat { line =>
          val fields = line.split("\t")
          if (fields.length >= 5) {
            NumberParser.parseInt(fields(1)) match {
              case Left(_) => Seq.empty
              case Right(pos) =>
                val chrom = fields(0)
                val ref = fields(3)
                val alt = fields(4)
                Seq(Variant(chrom, pos, ref, alt))
            }
          } else {
            Seq.empty
          }
        }.runFold(Map.empty[String, mutable.Builder[Variant, Seq[Variant]]]) { (variantsByChrom, variant) =>
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
}
