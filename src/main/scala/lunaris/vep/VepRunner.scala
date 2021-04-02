package lunaris.vep

import akka.stream.Materializer
import akka.stream.scaladsl.Source
import better.files.{File, Resource}
import lunaris.app.VepRunSettings
import lunaris.genomics.LociSet
import lunaris.io.{ExonsFileReader, FileInputId}
import lunaris.recipes.values.{LunType, LunValue}
import lunaris.streams.utils.RecordStreamTypes.Record
import lunaris.utils.{IOUtils, SnagUtils}
import lunaris.vep.vcf.VcfCore.VcfCoreRecord
import lunaris.vep.vcf.VcfStreamVariantsWriter
import lunaris.vep.vcf.VcfStreamVariantsWriter.VcfRecord
import org.broadinstitute.yootilz.core.snag.Snag

import java.io.PrintWriter
import java.nio.charset.StandardCharsets
import scala.sys.process._
import scala.util.Random

class VepRunner(val runSettings: VepRunSettings) {

  val vepWrapperScriptResourceShortName: String = "vepWrapper.sh"
  val vepWrapperScriptResourceFullName: String = "lunaris/vep/" + vepWrapperScriptResourceShortName

  val vepWorkDir: File = runSettings.workDir
  val vepWrapperScriptFile: File = vepWorkDir / vepWrapperScriptResourceShortName
  val vepRunDir: File = vepWorkDir / "run"

  val exonsFile: File = runSettings.exonsFile

  {
    vepWorkDir.createDirectoryIfNotExists(createParents = true)
    vepRunDir.createDirectoryIfNotExists(createParents = true)
    val vepScriptFileOut = vepWrapperScriptFile.newOutputStream
    IOUtils.writeAllYouCanRead(Resource.getAsStream(vepWrapperScriptResourceFullName), vepScriptFileOut)
  }

  val exonsSet: LociSet = SnagUtils.throwIfSnag(ExonsFileReader.read(FileInputId(exonsFile)))

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

  def processRecord(record: Record)(implicit materializer: Materializer): Either[Snag, Record] = {
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
      vcfRecord = VcfCoreRecord(chrom, pos, id, ref, alt, qual, filter, info, format)
      result <- calculateJoined(record, vcfRecord)
    } yield result
  }

  def runVep(inputFile: File, outputFile: File, warningsFile: File): Int = {
    val vepCmd = runSettings.vepCmd
    val cpus = 1
    val fastaFile = runSettings.fastaFile
    val cacheDir = runSettings.cacheDir
    val pluginsDir = runSettings.pluginsDir
    val dbNsfp = runSettings.dbNSFPFile
    val commandLine =
      s"bash $vepWrapperScriptFile $vepCmd $inputFile $cpus $fastaFile $cacheDir $pluginsDir $dbNsfp " +
        s"$outputFile $warningsFile"
    commandLine.!
  }

  val colNamePick: String = "PICK"

  private def createUseDiscardDir[T](dir: File)(user: File => T): T = {
    dir.createDirectory()
    val result = user(dir)
    dir.delete(swallowIOExceptions = true)
    result
  }

  def calculateValues(vcfRecord: VcfRecord)(implicit materializer: Materializer):
  Either[Snag, (Array[String], Array[String])] = {
    if (exonsSet.overlapsLocus(vcfRecord.toLocus)) {
      createUseDiscardDir(vepRunDir / Random.nextLong(Long.MaxValue).toHexString) { subRunDir =>
        val inputFile = subRunDir / "input.vcf"
        inputFile.bufferedWriter(StandardCharsets.UTF_8).map(new PrintWriter(_)).foreach { printWriter =>
          VcfStreamVariantsWriter.writeVcfRecords(Source.single(vcfRecord), printWriter)
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
              Left(Snag(s"vep produced no data line for variant ${vcfRecord.id}."))
            }
          } else {
            Left(Snag(s"vep produced no header line for variant ${vcfRecord.id}."))
          }
        } else {
          Left(Snag(s"vep return value was non-zero ($returnValue) for variant ${vcfRecord.id}."))
        }
      }
    } else {
      Left(Snag(s"${vcfRecord.toVariant.toCanonicalId} is not in the exome."))
    }
  }

  def calculateJoined(record: Record, vcfRecord: VcfRecord)(implicit materializer: Materializer):
  Either[Snag, Record] = {
    calculateValues(vcfRecord).flatMap { headersAndValues =>
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
  def createNewVepRunner(runSettings: VepRunSettings): VepRunner = new VepRunner(runSettings)
}
