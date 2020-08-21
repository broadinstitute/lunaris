package lunaris.streams

import akka.stream.SourceShape
import akka.stream.scaladsl.{GraphDSL, MergeSorted, Source}
import lunaris.genomics.{Locus, LocusOrdering}
import lunaris.recipes.values.{LunValue, RecordStreamWithMeta}
import lunaris.streams.utils.StreamTagger
import lunaris.streams.utils.StreamTagger.TaggedItem
import lunaris.utils.SeqBasedOrdering
import lunaris.streams.utils.RecordStreamTypes.{Record, Meta, RecordSource}

object RecordStreamMerger {

  private def getTaggedRecordOrdering(chroms: Seq[String]): Ordering[TaggedItem[Record, Int]] = {
    val chromosomeOrdering = SeqBasedOrdering(chroms)
    new Ordering[TaggedItem[Record, Int]] {
      val locusOrdering: LocusOrdering = new LocusOrdering(chromosomeOrdering)

      override def compare(tr1: TaggedItem[Record, Int], tr2: TaggedItem[Record, Int]): Int =
        locusOrdering.compare(tr1.item.locus, tr2.item.locus)
    }
  }

  private class TaggedRecordMerger(nStreams: Int) {
    val locusByStream: Array[Option[Locus]] = Array.fill(nStreams)(None)
    var records: Seq[TaggedItem[Record, Int]] = Seq.empty
    val streamIsExhausted: Array[Boolean] = Array.fill(nStreams)(false)

    private def enterRecord(record: TaggedItem[Record, Int]): Unit = {
      records :+= record
      val iStream = record.sourceId
      streamIsExhausted(iStream) = record.isLast
      locusByStream(iStream) = Some(record.item.locus)
    }

    private def streamHasNoMoreForLocus(iStream: Int, locus: Locus): Boolean = {
      locusByStream(iStream) match {
        case Some(locusForStream) => locusForStream != locus
        case None => streamIsExhausted(iStream)
      }
    }

    private def noStreamHasMoreForLocus(locus: Locus): Boolean = {
      (1 until nStreams).forall(streamHasNoMoreForLocus(_, locus))
    }

    private def extractJoinedRecord(): Record = {
      val firstRecord = records.head.item
      val (sameRecords, otherRecords) =
        records.partition(record => (record.item.id == firstRecord.id) && (record.item.locus == firstRecord.locus))
      records = otherRecords
      sameRecords.sortBy(_.sourceId).map(_.item).reduce((o1, o2) => o1.joinWith(o2).toOption.get)
    }

    def addNext(taggedRecord: TaggedItem[Record, Int]): Seq[Record] = {
      enterRecord(taggedRecord)
      val builder = Seq.newBuilder[Record]
      while (records.nonEmpty && noStreamHasMoreForLocus(records.head.item.locus)) {
        builder += extractJoinedRecord()
      }
      builder.result()
    }
  }

  def merge(meta: Meta, sources: Seq[RecordSource]): RecordSource = {
    val taggedSources = sources.zipWithIndex.collect {
      case (source, iSource) => StreamTagger.tagSource(source, iSource)
    }
    val mergedTaggedSource = Source.fromGraph(GraphDSL.create(taggedSources) { implicit builder =>
      sources =>
        import GraphDSL.Implicits._
        if (sources.size == 1) {
          sources.head
        } else {
          val source0 :: source1 :: tail = sources
          implicit val taggedRecordOrdering: Ordering[TaggedItem[Record, Int]]
          = getTaggedRecordOrdering(meta.chroms)
          var merged = builder.add(new MergeSorted[TaggedItem[Record, Int]]())
          source0.out ~> merged.in0
          source1.out ~> merged.in1
          for (oSource <- tail) {
            val mergedNew = builder.add(new MergeSorted[TaggedItem[Record, Int]]())
            merged.out ~> mergedNew.in0
            oSource ~> mergedNew.in1
            merged = mergedNew
          }
          SourceShape(merged.out)
        }
    })
    val taggedRecordMerger = new TaggedRecordMerger(sources.size)
    mergedTaggedSource.statefulMapConcat(() => taggedRecordMerger.addNext).mapMaterializedValue(_ => meta)
  }
}
