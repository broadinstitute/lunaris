package lunaris.vep

import akka.Done
import akka.stream.Materializer
import akka.stream.scaladsl.{Sink, Source}
import better.files.{File, Resource}
import lunaris.app.VepRunSettings
import lunaris.genomics.LociSet
import lunaris.io.{ExonsFileReader, FileInputId, ResourceConfig}
import lunaris.recipes.values.LunValue
import lunaris.streams.utils.RecordStreamTypes.Record
import lunaris.utils.{IOUtils, SnagUtils}
import lunaris.vep.vcf.VcfCore.VcfCoreRecord
import lunaris.vep.vcf.VcfStreamVariantsWriter
import lunaris.vep.vcf.VcfStreamVariantsWriter.VcfRecord
import org.broadinstitute.yootilz.core.snag.Snag

import java.io.PrintWriter
import java.nio.charset.StandardCharsets
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.sys.process._
import scala.util.{Failure, Random, Success}

class VepRunner(val runSettings: VepRunSettings) {

  private val vepWrapperScriptResourceShortName: String = "vepWrapper.sh"
  private val vepWrapperScriptResourceFullName: String = "lunaris/vep/" + vepWrapperScriptResourceShortName

  private val vepWorkDir: File = runSettings.workDir
  private val vepWrapperScriptFile: File = vepWorkDir / vepWrapperScriptResourceShortName
  private val vepRunDir: File = runSettings.runDir

  private val exonsFile: File = runSettings.exonsFile

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

  def processRecord(record: Record, snagLogger: Snag => ())(implicit materializer: Materializer):
  Either[Snag, Record] = {
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
      result <- calculateJoined(record, vcfRecord, snagLogger)
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

  private def createUseDiscardDir[T](dir: File)(user: File => T): T = {
    dir.createDirectory()
    val result = user(dir)
    dir.delete(swallowIOExceptions = true)
    result
  }

  def writeVepInputFile[M](inputFile: File, vcfRecords: Source[VcfRecord, M])(
    implicit materializer: Materializer): Future[Done] = {
    inputFile.bufferedWriter(StandardCharsets.UTF_8).map(new PrintWriter(_)) { printWriter =>
      VcfStreamVariantsWriter.writeVcfRecords(vcfRecords, printWriter)
    }
  }

  def calculateValues(vcfRecord: VcfRecord, snagLogger: Snag => ())(implicit materializer: Materializer):
  Either[Snag, Record] = {
    createUseDiscardDir(vepRunDir / Random.nextLong(Long.MaxValue).toHexString) { subRunDir =>
      val vcfRecords = Source.single(vcfRecord)
      val chroms = Seq(vcfRecord.chrom)
      val inputFile = subRunDir / "input.vcf"
      val doneFut = writeVepInputFile(inputFile, vcfRecords)
      val outputFile = subRunDir / "output.tsv"
      val warningsFile = subRunDir / "warnings"
      Await.ready(doneFut, Duration.Inf) //  TODO do without Await
      val returnValue = runVep(inputFile, outputFile, warningsFile)
      if (returnValue == 0) {
        val records = VepOutputReader.read(inputFile, ResourceConfig.empty, chroms, snagLogger)
        val recordSeqFut = records.runWith(Sink.collection[Record, Seq[Record]])
        Await.ready(recordSeqFut, Duration.Inf) //  TODO do without Await
        recordSeqFut.value.get match {
          case Failure(exception) =>
            Left(Snag(exception))
          case Success(recordSeq) =>
            recordSeq.headOption match {
              case Some(record) => Right(record)
              case None => Left(Snag(s"VEP produced no output for ${vcfRecord.id}."))
            }
        }
      } else {
        Left(Snag(s"vep return value was non-zero ($returnValue) for variant ${vcfRecord.id}."))
      }
    }
  }

  def calculateJoined(record: Record, vcfRecord: VcfRecord, snagLogger: Snag => ())(
    implicit materializer: Materializer):
  Either[Snag, Record] = {
    calculateValues(vcfRecord, snagLogger).flatMap { valuesRecord =>
      record.joinWith(valuesRecord)
    }
  }
}

object VepRunner {
  def createNewVepRunner(runSettings: VepRunSettings): VepRunner = new VepRunner(runSettings)
}
