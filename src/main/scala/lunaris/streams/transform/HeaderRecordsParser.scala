package lunaris.streams.transform

import akka.stream.scaladsl.{Framing, Sink, Source}
import akka.stream.{IOResult, Materializer}
import akka.util.ByteString
import lunaris.io.{InputId, ResourceConfig}
import lunaris.recipes.values.RecordStreamWithMeta.Meta
import lunaris.recipes.values.{LunType, RecordStreamWithMeta}
import lunaris.streams.utils.RecordStreamTypes.Record
import org.broadinstitute.yootilz.core.snag.{Snag, SnagException}

import scala.collection.immutable.ArraySeq
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, ExecutionContextExecutor, Future}

object HeaderRecordsParser {

  private def getLines(input: InputId, resourceConfig: ResourceConfig): Source[String, Future[IOResult]] = {
    input.newStream(resourceConfig)
      .via(Framing.delimiter(ByteString("\n"), Int.MaxValue, allowTruncation = true))
      .map(_.utf8String)
  }

  private def parseHeaderLine[I](recordCoreType: LunType.RecordType, headerLine: String)
                                (indexer: Seq[String] => Either[Snag, I]):
  Either[Snag, (LunType.RecordType, I)] = {
    val headerLineCleaned = if (headerLine.startsWith("#")) headerLine.substring(1) else headerLine
    val cols = headerLineCleaned.split("\t")
    val recordType = {
      var recordTypeTmp = recordCoreType
      recordTypeTmp = recordTypeTmp.copy(fields = cols)
      for (col <- cols) {
        if(!recordTypeTmp.elementTypes.contains(col)) {
          recordTypeTmp = recordTypeTmp.copy(elementTypes = recordTypeTmp.elementTypes + (col -> LunType.StringType))
        }
      }
      recordTypeTmp
    }
    indexer(ArraySeq.unsafeWrapArray(cols)).map((recordType, _))
  }

  private def getRecordType[I](input: InputId, resourceConfig: ResourceConfig)
                              (recordCoreType: LunType.RecordType)
                              (indexer: Seq[String] => Either[Snag, I])
                              (implicit materializer: Materializer):
  Either[Snag, (LunType.RecordType, I)] = {
    implicit val executionContext: ExecutionContext = materializer.executionContext
    val headerLineOptFut = getLines(input, resourceConfig)
      .filter(!_.startsWith("##"))
      .take(1)
      .runWith(Sink.collection[String, Seq[String]])
      .map(_.headOption)
    val headerLineOpt = Await.result(headerLineOptFut, Duration.Inf)
    headerLineOpt match {
      case Some(headerLine) => parseHeaderLine(recordCoreType, headerLine)(indexer)
      case None => Left(Snag(s"Missing header line in $input."))
    }
  }

  def newRecordSource[I](input: InputId, resourceConfig: ResourceConfig, chroms: Seq[String])
                        (recordCoreType: LunType.RecordType)
                        (indexer: Seq[String] => Either[Snag, I])
                        (recordParser: (LunType.RecordType, I, Seq[String]) => Either[Snag, Record])
                        (snagLogger: Snag => ())
                        (implicit materializer: Materializer): RecordStreamWithMeta = {
    implicit val executionContext: ExecutionContextExecutor = materializer.executionContext
    getRecordType(input, resourceConfig)(recordCoreType)(indexer) match {
      case Left(snag) =>
        val meta = Meta(recordCoreType, chroms)
        val stream = Source.failed(SnagException(snag)).mapMaterializedValue(_ => Meta(recordCoreType, chroms))
        RecordStreamWithMeta(meta, stream)
      case Right((recordType, index)) =>
        val meta = Meta(recordType, chroms)
        val stream = getLines(input, resourceConfig)
          .filter(!_.startsWith("#"))
          .map(_.split("\t"))
          .map { fields =>
            recordParser(recordType, index, ArraySeq.unsafeWrapArray(fields))
          }
          .mapConcat { snagOrRecord =>
            snagOrRecord.left.foreach(snagLogger)
            snagOrRecord.toSeq
          }
          .mapMaterializedValue(_ => meta)
        RecordStreamWithMeta(meta, stream)
    }
  }
}
