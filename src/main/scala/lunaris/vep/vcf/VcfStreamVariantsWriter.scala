package lunaris.vep.vcf

import akka.Done
import akka.stream.Materializer
import akka.stream.scaladsl.{Keep, Sink, Source}
import better.files.File

import java.io.PrintWriter
import java.nio.charset.StandardCharsets
import scala.concurrent.Future

object VcfStreamVariantsWriter {

  type VcfRecord = VcfCore.VcfCoreRecord

  def writeVcfRecords[M](records: Source[VcfRecord, M], writer: PrintWriter)(implicit materializer: Materializer):
  Future[Done] = {
    val lines = Source.single(VcfCore.ColNames.headerLine).concatMat(records.map(_.toLine))(Keep.right)
    lines.runWith(Sink.foreach(writer.println))
  }

}
