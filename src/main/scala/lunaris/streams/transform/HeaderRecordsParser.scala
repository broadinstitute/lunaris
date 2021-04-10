package lunaris.streams.transform

import akka.stream.scaladsl.{Framing, Sink, Source}
import akka.stream.{IOResult, Materializer}
import akka.util.ByteString
import lunaris.io.{InputId, ResourceConfig}
import lunaris.recipes.values.LunType
import lunaris.recipes.values.RecordStreamWithMeta.Meta
import lunaris.streams.utils.RecordStreamTypes.{Record, RecordSource}
import org.broadinstitute.yootilz.core.snag.{Snag, SnagException}

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, ExecutionContextExecutor, Future}

object HeaderRecordsParser {

  type RecordParser = String => Either[Snag, Record]
  type RecordParserGenerator = (LunType.RecordType, Array[String]) => Either[Snag, RecordParser]

  private def getLines(input: InputId, resourceConfig: ResourceConfig): Source[String, Future[IOResult]] = {
    input.newStream(resourceConfig)
      .via(Framing.delimiter(ByteString("\n"), Int.MaxValue, allowTruncation = true))
      .map(_.utf8String)
  }

  private def checkCoreFields(cols: Array[String], coreFields: Seq[String]): Either[Snag, ()] = {
    var snagOpt: Option[Snag] = None
    val coreFieldsIter = coreFields.iterator
    while (snagOpt.isEmpty && coreFieldsIter.hasNext) {
      val coreField = coreFieldsIter.next()
      if (!cols.contains(coreField)) {
        snagOpt = Some(Snag(s"Missing column $coreField."))
      }
    }
    snagOpt.fold[Either[Snag, ()]](Right(()))(Left(_))
  }

  private def parseHeaderLine(recordCoreType: LunType.RecordType, headerLine: String)
                             (recordParserGenerator: RecordParserGenerator):
  Either[Snag, (LunType.RecordType, RecordParser)] = {
    val headerLineCleaned = if (headerLine.startsWith("#")) headerLine.substring(1) else headerLine
    val cols = headerLineCleaned.split("\t")
    checkCoreFields(cols, recordCoreType.fields) match {
      case Left(snag) => Left(snag)
      case Right(_) =>
        var recordTypeTmp = recordCoreType
        for (col <- cols) {
          if (!recordTypeTmp.fields.contains(col)) {
            recordTypeTmp = recordTypeTmp.addField(col, LunType.StringType)
          }
        }
        recordParserGenerator(recordTypeTmp, cols).map(recordParser => (recordTypeTmp, recordParser))
    }
  }

  private def getRecordType(input: InputId, resourceConfig: ResourceConfig)
                           (recordCoreType: LunType.RecordType)
                           (recordParserGenerator: RecordParserGenerator)
                           (implicit materializer: Materializer):
  Either[Snag, (LunType.RecordType, RecordParser)] = {
    implicit val executionContext: ExecutionContext = materializer.executionContext
    val headerLineOptFut = getLines(input, resourceConfig)
      .filter(!_.startsWith("##"))
      .take(1)
      .runWith(Sink.collection[String, Seq[String]])
      .map(_.headOption)
    val headerLineOpt = Await.result(headerLineOptFut, Duration.Inf)
    headerLineOpt match {
      case Some(headerLine) => parseHeaderLine(recordCoreType, headerLine)(recordParserGenerator)
      case None => Left(Snag(s"Missing header line in $input."))
    }
  }

  def newRecordSource(input: InputId, resourceConfig: ResourceConfig, chroms: Seq[String])
                     (recordCoreType: LunType.RecordType)
                     (recordParserGenerator: RecordParserGenerator)
                     (snagLogger: Snag => ())
                     (implicit materializer: Materializer): RecordSource = {
    implicit val executionContext: ExecutionContextExecutor = materializer.executionContext
    getRecordType(input, resourceConfig)(recordCoreType)(recordParserGenerator) match {
      case Left(snag) =>
        Source.failed(SnagException(snag)).mapMaterializedValue(_ => Meta(recordCoreType, chroms))
      case Right((recordType, recordParser)) =>
        val meta = Meta(recordType, chroms)
        getLines(input, resourceConfig)
          .filter(!_.startsWith("#"))
          .map(recordParser)
          .mapConcat { snagOrRecord =>
            snagOrRecord.left.foreach(snagLogger)
            snagOrRecord.toSeq
          }
          .mapMaterializedValue(_ => meta)
    }
  }
}
