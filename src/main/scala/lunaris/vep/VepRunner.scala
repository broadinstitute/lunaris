package lunaris.vep

import java.io.PrintWriter
import java.nio.charset.StandardCharsets

import better.files.{File, Resource}
import lunaris.recipes.values.{LunType, LunValue}
import lunaris.streams.utils.RecordStreamTypes.Record
import lunaris.utils.IOUtils
import org.broadinstitute.yootilz.core.snag.Snag

import scala.sys.process._
import scala.util.Random

class VepRunner(val vepInstallation: VepInstallation) {

  val vepWrapperScriptResourceShortName: String = "vepWrapper.sh"
  val vepWrapperScriptResourceFullName: String = "lunaris/vep/" + vepWrapperScriptResourceShortName

  val vepWorkDir: File = vepInstallation.workDir
  val vepWrapperScriptFile: File = vepWorkDir / vepWrapperScriptResourceShortName
  val vepRunDir: File = vepWorkDir / "run"

  {
    vepWorkDir.createDirectoryIfNotExists(createParents = true)
    vepRunDir.createDirectoryIfNotExists(createParents = true)
    val vepScriptFileOut = vepWrapperScriptFile.newOutputStream
    IOUtils.writeAllYouCanRead(Resource.getAsStream(vepWrapperScriptResourceFullName), vepScriptFileOut)
  }

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

  def runVep(inputFile: File, outputFile: File, warningsFile: File): Int = {
    val vepScriptFile = vepInstallation.vepScriptFile
    val cpus = 1
    val fastaFile = vepInstallation.fastaFile
    val pluginsDir = vepInstallation.pluginsDir
    val dbNsfp = vepInstallation.dbNSFP
    // TODO remaining arguments
    val commandLine =
      s"bash $vepWrapperScriptFile $vepScriptFile $inputFile $cpus $fastaFile $pluginsDir $dbNsfp " +
        s"$outputFile $warningsFile"
    commandLine.!
  }

  val colNamePick: String = "PICK"

  def calculateValues(id: String, chrom: String, pos: Int, ref: String, alt: String, qual: String, filter: String,
                      info: String, format: String):
  Either[Snag, (Array[String], Array[String])] = {
    val subRunDir = vepRunDir / Random.nextLong(Long.MaxValue).toHexString
    subRunDir.createDirectory()
    val inputFile = subRunDir / "input.vcf"
    inputFile.bufferedWriter(StandardCharsets.UTF_8).map(new PrintWriter(_)).foreach { printWriter =>
      printWriter.println("#CHROM\tPOS\tID\tREF\tALT\tQUAL\tFILTER\tINFO\tFORMAT")
      printWriter.println(s"$chrom\t$pos\t$id\t$ref\t$alt\t$qual\t$filter\t$info\t$format")
    }
    val outputFile = subRunDir / "output.tsv"
    val warningsFile = subRunDir / "warnings"
    val returnValue = runVep(inputFile, outputFile, warningsFile)
    if (returnValue == 0) {
      val lineIter = outputFile.lineIterator.dropWhile(_.startsWith("##"))
      if (lineIter.hasNext) {
        val headerLineRaw = lineIter.next()
        val headerLine = if (headerLineRaw.startsWith("#")) {
          headerLineRaw.substring(1)
        } else {
          headerLineRaw
        }
        val headers = headerLine.split("\t")
        var valuesOpt: Option[Array[String]] = None
        val iPick = headers.indexOf(colNamePick)
        if (iPick >= 0) {
          val requiredPickValue = "1"
          while (lineIter.hasNext && valuesOpt.isEmpty) {
            val dataLine = lineIter.next()
            val values = dataLine.split("\t")
            if (values.length > iPick && values(iPick) == requiredPickValue) {
              valuesOpt = Some(values)
            }
          }
          valuesOpt match {
            case None => Left(Snag(s"vep produced no row with value $requiredPickValue for $colNamePick."))
            case Some(values) => Right((headers, values))
          }
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
  def default: VepRunner = new VepRunner(VepInstallation.autoPick)
}
