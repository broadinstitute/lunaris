package lunaris.vep

import akka.stream.scaladsl.{Framing, Source}
import akka.util.ByteString
import better.files.File
import lunaris.io.{FileInputId, InputId, ResourceConfig}
import lunaris.recipes.values.{LunType, LunValue, RecordStreamWithMeta}
import lunaris.streams.utils.RecordStreamTypes
import lunaris.streams.utils.RecordStreamTypes.RecordSource

import java.nio.charset.StandardCharsets

object VepOutputReader {

  object ColNames {
    val id = "Uploaded_variation"
    val chrom = "Chrom"
    val pos = "Pos"
    val ref = "Ref"
    val alt = "Alt"
  }

  def read(inputFile: File, resourceConfig: ResourceConfig, chroms: Seq[String]): RecordSource = {
    val lineIter = inputFile.lineIterator(StandardCharsets.UTF_8).filter(!_.startsWith("##"))
    val recordTypeCore = LunType.RecordType(ColNames.id, ColNames.chrom, ColNames.pos, ColNames.pos)
    if(lineIter.hasNext) {
      val headerLineRaw = lineIter.next()
      val headerLine = if(headerLineRaw.startsWith("#")) headerLineRaw.substring(1) else headerLineRaw
      val headers = headerLine.split("\t")
      FileInputId(inputFile).newStream(resourceConfig)
        .via(Framing.delimiter(ByteString("\n"), Int.MaxValue, allowTruncation = true))
        .map(_.utf8String)
        .map { line =>
          val valueStrings = line.split("\t")
          var record = LunValue.RecordValue
          for (i <- 0 until Math.min(headers.length, valueStrings.length)) {

          }
          ???
        }
      ???
    } else {
      val meta = RecordStreamWithMeta.Meta(recordType, chroms)
      Source.failed(new Exception(s"File $inputFile has no header line")).mapMaterializedValue(_ => meta)
    }
  }

}
