package lunaris.vep

import java.io.PrintWriter
import java.nio.charset.StandardCharsets

import better.files.File
import lunaris.recipes.values.{LunType, LunValue}
import lunaris.streams.utils.RecordStreamTypes.Record
import org.broadinstitute.yootilz.core.snag.Snag

import scala.sys.process._
import scala.util.Random

class VepRunner(val vepRunPath: File, val vepWorkDir: File) {

  vepWorkDir.createDirectoryIfNotExists()

  private def optToSnagOrValue[T](itemOpt: Option[T])(snagMessage: => String): Either[Snag, T] = {
    itemOpt match {
      case Some(item) => Right(item)
      case None => Left(Snag(snagMessage))
    }
  }

  private def optionalValueOrEmptyString(valueOpt: Option[LunValue]): String = {
    valueOpt.map(_.asString) match {
      case Some(Right(value)) => value
      case _ => ""
    }
  }

  def processRecord(record: Record): Either[Snag, Record] = {
    val id = record.id
    val chrom = record.locus.chrom
    val pos = record.locus.region.begin
    val qual = optionalValueOrEmptyString(record.values.get("QUAL"))
    val filter = optionalValueOrEmptyString(record.values.get("FILTER"))
    val info = optionalValueOrEmptyString(record.values.get("INFO"))
    val format = optionalValueOrEmptyString(record.values.get("FORMAT"))
    for {
      refValue <- optToSnagOrValue(record.values.get("REF"))("No value for REF.")
      ref <- refValue.asString
      altValue <- optToSnagOrValue(record.values.get("ALT"))("No value for ALT.")
      alt <- altValue.asString
      result <- calculateJoined(record, id, chrom, pos, ref, alt, qual, filter, info, format)
    } yield result
  }

  def calculateValues(id: String, chrom: String, pos: Int, ref: String, alt: String, qual: String, filter: String,
                      info: String, format: String):
  Either[Snag, (Array[String], Array[String])] = {
    val subWorkDir = vepWorkDir / Random.nextLong(Long.MaxValue).toHexString
    subWorkDir.createDirectory()
    val inputFile = subWorkDir / "input.vcf"
    inputFile.bufferedWriter(StandardCharsets.UTF_8).map(new PrintWriter(_)).foreach { printWriter =>
      printWriter.println("#CHROM\tPOS\tID\tREF\tALT\tQUAL\tFILTER\tINFO\tFORMAT")
      printWriter.println(s"$chrom\t$pos\t$id\t$ref\t$alt\t$qual\t$filter\t$info\t$format")
    }
    val outputFile = subWorkDir / "output.tsv"
    val returnValue = s"$vepRunPath --cache -i $inputFile -o $outputFile".!
    if(returnValue == 0) {
      val lineIter = outputFile.lineIterator.dropWhile(_.startsWith("##"))
      if(lineIter.hasNext) {
        val headerLineRaw = lineIter.next()
        val headerLine = if(headerLineRaw.startsWith("#")) {
          headerLineRaw.substring(1)
        } else {
          headerLineRaw
        }
        if(lineIter.hasNext) {
          val dataLine = lineIter.next()
          val headers = headerLine.split("\t")
          val values = dataLine.split("\t")
          Right((headers, values))
        } else {
          Left(Snag(s"vep produced no data line for variant $id"))
        }
      } else {
        Left(Snag(s"vep produced no header line for variant $id"))
      }
    } else {
      Left(Snag(s"vep return value was non-zero ($returnValue)."))
    }
  }

  def calculateJoined(record: Record, id: String, chrom: String, pos: Int, ref: String, alt: String, qual: String,
                      filter: String, info: String, format: String): Either[Snag, Record] = {
    calculateValues(id, chrom, pos, ref, alt, qual, filter, info, format).flatMap { headersAndValues =>
      val (headers, values) = headersAndValues
      val valuesRecordType = {
        var typeTmp = record.lunType
        headers.foreach { header =>
          typeTmp = typeTmp.addField(header, LunType.StringType)
        }
        typeTmp
      }
      val valuesValues = values.map(LunValue.PrimitiveValue.StringValue)
      val valuesMap = headers.zip(valuesValues).toMap
      val valuesRecord = LunValue.RecordValue(record.id, record.locus, valuesRecordType, valuesMap)
      record.joinWith(valuesRecord)
    }
  }
}

object VepRunner {
  def default: VepRunner = {
    val vepRunPath = File("/home/BROAD.MIT.EDU/oliverr/git/ensembl-vep/vep")
    val vepWorkDir = File("/home/BROAD.MIT.EDU/oliverr/lunaris/vep/run")
    new VepRunner(vepRunPath, vepWorkDir)
  }
}
