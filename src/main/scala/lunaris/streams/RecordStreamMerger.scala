package lunaris.streams

import akka.stream.scaladsl.{GraphDSL, MergeSorted, Source}
import lunaris.genomics.LocusOrdering
import lunaris.recipes.values.{LunValue, RecordStream}

object RecordStreamMerger {

  private case class TaggedRecord(record: LunValue.ObjectValue, isFromFirstSource: Boolean, isLast: Boolean)

  private def taggedRecordOrdering(chromosomeOrdering: Ordering[String]): Ordering[TaggedRecord] = {
    new Ordering[TaggedRecord] {
      val locusOrdering: LocusOrdering = new LocusOrdering(chromosomeOrdering)
      override def compare(tr1: TaggedRecord, tr2: TaggedRecord): Int =
        locusOrdering.compare(tr1.record.locus, tr2.record.locus)
    }
  }

  private def taggedStream(source: Source[LunValue.ObjectValue, RecordStream.Meta], isFromFirstStream: Boolean):
  Source[TaggedRecord, RecordStream.Meta] = {
    source.map(Some(_)).concat(Source.single(None)).sliding(2).map { recordOpts =>
      val isLast = recordOpts.size < 2 || recordOpts(1).isEmpty
      TaggedRecord(recordOpts.head.get, isFromFirstStream, isLast)
    }
  }

  def merge(source1: Source[LunValue.ObjectValue, RecordStream.Meta],
            source2: Source[LunValue.ObjectValue, RecordStream.Meta]):
  Source[LunValue.ObjectValue, RecordStream.Meta] = {
    val taggedSource1 = taggedStream(source1, true)
    val taggedSource2 = taggedStream(source2, false)
    val taggedMerged = GraphDSL.create() { implicit builder =>
      import GraphDSL.Implicits._

      val merge = builder.add(new MergeSorted())
    }
    ???
  }

}
