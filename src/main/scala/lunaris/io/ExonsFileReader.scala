package lunaris.io

import lunaris.genomics.{LociSet, Locus, Region}
import lunaris.io
import lunaris.utils.{Eitherator, NumberParser}
import org.broadinstitute.yootilz.core.snag.{Snag, SnagTag}

object ExonsFileReader {
  def read(in: InputId, resourceConfig: ResourceConfig = ResourceConfig.empty): Either[Snag, LociSet] = {
    in.newReadChannelDisposable(resourceConfig).useUp { readChannel =>
      val bufferSize = 10000
      val refiller = new ByteBufferRefiller.FromChannel(readChannel, bufferSize)
      val reader = ByteBufferReader(refiller)
      val lineEitherator = Eitherator.fromGeneratorUntilTag(SnagTag.endOfData)(reader.readLine())
      val lociEitherator = lineEitherator.process { line =>
        val parts = line.split("\t")
        if(parts.length != 4) {
          Left(Snag(s"Line should contain 4 fields, but has ${parts.length}: $line"))
        } else {
          for {
            chrom <- Right(parts(1))
            begin <- NumberParser.parseInt(parts(2))
            end <- NumberParser.parseInt(parts(3))
          } yield Some(Locus(chrom, Region(begin, end)))
        }
      }
      val lociSetBuilder = LociSet.newBuilder
      val forEachResult = lociEitherator.foreach { locus =>
        lociSetBuilder += locus
      }
      forEachResult match {
        case Left(snag) => Left(snag)
        case _ => Right(lociSetBuilder.result())
      }
    }
  }
}
