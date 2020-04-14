package lunaris.io.query

import lunaris.data.DataSourceWithIndex
import lunaris.genomics.Region
import lunaris.io.tbi.{TBIBins, TBIChunk, TBIFileReader}
import lunaris.io.{ByteBufferReader, ByteBufferRefiller, IntegersIO, ResourceConfig}
import lunaris.stream.Record
import lunaris.utils.Eitherator
import org.broadinstitute.yootilz.core.snag.Snag

object RecordExtractor {

  def extract(dataSourceWithIndex: DataSourceWithIndex,
              regionsBySequence: Map[String, Seq[Region]]): Eitherator[Record] = {
    dataSourceWithIndex.index.newReadChannelDisposable(ResourceConfig.empty).useUp { indexReadChannel =>
      println("Now extracting records")
      val bufferSize = 10000
      val indexReader = new ByteBufferReader(ByteBufferRefiller.bgunzip(indexReadChannel, bufferSize))
      val tbiConsumer = new RecordTbiConsumer(regionsBySequence, new LimitedLogger(50000))
      TBIFileReader.readFile(indexReader, tbiConsumer)
      println("Done extracting records!")
      Eitherator.empty
    }
  }

  class LimitedLogger(limit: Int) extends (String => Unit) {
    var count: Int = 0

    override def apply(line: String): Unit = {
      if (count < limit) {
        println(line)
        count += 1
      }
    }
  }

  case class SequenceInfo(name: String, regions: Seq[Region], binsByRegion: Map[Region, Set[Int]])

  object SequenceInfo {
    def empty: SequenceInfo = SequenceInfo("", Seq.empty, Map.empty)

    def apply(name: String, regions: Seq[Region]): SequenceInfo = {
      val bins = TBIBins.binsOverlappingRegions(regions)
      new SequenceInfo(name, regions, bins)
    }
  }

  class RecordTbiConsumer(val regionsBySequence: Map[String, Seq[Region]],
                          logger: String => Unit) extends TBIFileReader.TBIConsumer {
    var snagOpt: Option[Snag] = None

    override def consumeChunksForSequence(name: String, chunksByRegion: Map[Region, Seq[TBIChunk]]): Unit = {
      logger(s"Sequence: $name, chunks by region: $chunksByRegion.")
    }

    override def consumeSnag(snag: Snag): Unit = {
      snagOpt = Some(snag)
      println("There was a problem reading the TBI file!")
      println(snag.message)
      println(snag.report)
    }

  }

}
