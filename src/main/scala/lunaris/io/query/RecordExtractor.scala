package lunaris.io.query

import lunaris.data.DataSourceWithIndex
import lunaris.genomics.{Chromosome, Region, Regions}
import lunaris.io.tbi.{TBIBins, TBIFileHeader, TBIFileReader, TbiVirtualFileOffset}
import lunaris.io.{ByteBufferReader, ByteBufferRefiller, IntegersIO, ResourceConfig}
import lunaris.stream.Record
import lunaris.utils.Eitherator
import org.broadinstitute.yootilz.core.snag.Snag

object RecordExtractor {

  def extract(dataSourceWithIndex: DataSourceWithIndex, regions: Regions): Eitherator[Record] = {
    dataSourceWithIndex.index.newReadChannelDisposable(ResourceConfig.empty).useUp { indexReadChannel =>
      println("Now extracting records")
      val bufferSize = 10000
      val indexReader = new ByteBufferReader(ByteBufferRefiller.bgunzip(indexReadChannel, bufferSize))
      val tbiConsumer = new RecordTbiConsumer(regions, new LimitedLogger(50000))
      TBIFileReader.readFile(indexReader, tbiConsumer)
      println("Done extracting records!")
      Eitherator.empty
    }
  }

  class LimitedLogger(limit: Int) extends (String => Unit) {
    var count: Int = 0
    override def apply(line: String): Unit = {
      if(count < limit) {
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

  class RecordTbiConsumer(regions: Regions, logger: String => Unit) extends TBIFileReader.TBIConsumer {
    var snagOpt: Option[Snag] = None
    var sequenceInfo: SequenceInfo = SequenceInfo.empty
    override def consumeHeader(header: TBIFileHeader): Unit = {
      logger("TBI file header: " + header)
    }

    override def startSequenceIndex(name: String): Unit = {
      Chromosome.parse(name) match {
        case None => consumeSnag(Snag(s"Cannot parse chromosome name $name."))
        case Some(chromosome) =>
          val regionsForChromosome = regions.regionsByChromosome.getOrElse(chromosome, Seq.empty)
          val bins = TBIBins.binsOverlappingRegions(regionsForChromosome)
          sequenceInfo = SequenceInfo(name, regionsForChromosome, bins)
          logger("Starting sequence index for " + name)
      }
    }

    override def regionsOverlappingBin(bin: IntegersIO.UnsignedInt): Set[Region] = {
      val uBin = bin.toPositiveIntOpt.get
      sequenceInfo.binsByRegion.collect {
        case (region, bins) if bins(uBin) => region
      }.toSet
    }

    override def consumeChunks(chunks: Seq[TBIFileReader.Chunk]): Unit = {
      logger("Chunks: " + chunks)
    }

    override def consumeNIntervals(nIntervals: Int): Unit = {
      logger("Number of intervals is " + nIntervals)
    }

    override def consumeIntervalOffset(offset: TbiVirtualFileOffset): Unit = {
      logger("Interval offset is : " + offset)
    }

    override def doneWithSequenceIndex(name: String): Unit = {
      logger("Done with sequence index for " + name)
    }

    override def consumeSnag(snag: Snag): Unit = {
      snagOpt = Some(snag)
      println("There was a problem reading the TBI file!")
      println(snag.message)
      println(snag.report)
    }
  }

}
