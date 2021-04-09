package lunaris.streams.transform

import akka.stream.{IOResult, Materializer}
import akka.stream.scaladsl.{Framing, Sink, Source}
import akka.util.ByteString
import lunaris.io.{InputId, ResourceConfig}
import lunaris.streams.utils.RecordStreamTypes.{Meta, Record, RecordSource}
import org.broadinstitute.yootilz.core.snag.{Snag, SnagException}

import scala.concurrent.{ExecutionContextExecutor, Future}

object HeaderRecordsParser {

  class LineProcessor[H](isPreHeader: String => Boolean)
                        (headerParser: String => Either[Snag, H])
                        (recordParser: (H, String) => Either[Snag, Record])
                        (snagLogger: Snag => ())
    extends (String => Seq[Record]) {
    var headerOpt: Option[H] = None

    override def apply(line: String): Seq[Record] = {
      headerOpt match {
        case None =>
          if(isPreHeader(line)) {
            Seq()
          } else {
            headerParser(line) match {
              case Left(snag) =>
                snagLogger(Snag("Could not parse header line", snag))
                throw new SnagException(snag)
              case Right(header) =>
                headerOpt = Some(header)
                Seq()
            }
          }
        case Some(header) =>
          val snagOrRecord = recordParser(header, line)
          snagOrRecord.left.foreach(snagLogger)
          snagOrRecord.toSeq
      }
    }
  }

  private def getLines(input: InputId, resourceConfig: ResourceConfig): Source[String, Future[IOResult]] = {
    input.newStream(resourceConfig)
      .via(Framing.delimiter(ByteString("\n"), Int.MaxValue, allowTruncation = true))
      .map(_.utf8String)
  }

  def parseRecords[H](input: InputId, resourceConfig: ResourceConfig, meta: Meta)
                     (isPreHeader: String => Boolean)
                     (headerParser: String => Either[Snag, H])
                     (recordParser: (H, String) => Either[Snag, Record])
                     (snagLogger: Snag => ())
                     (implicit materializer: Materializer): RecordSource = {
    implicit val executionContext: ExecutionContextExecutor = materializer.executionContext
    getLines(input, resourceConfig)
      .filter(!isPreHeader(_))
      .take(1)
      .runWith(Sink.collection)
      .map(_.headOption)
      .map {
        case None =>
          val snag = Snag("Missing header line")
          snagLogger(snag)
          throw new SnagException(snag)
        case Some(headerLine) =>

      }

    getLines(input, resourceConfig).flatMapPrefix()

//    Source.future()


    getLines(input, resourceConfig)
      .statefulMapConcat(() => new LineProcessor[H](isPreHeader)(headerParser)(recordParser)(snagLogger))
      .mapMaterializedValue(_ => meta)
  }
}
