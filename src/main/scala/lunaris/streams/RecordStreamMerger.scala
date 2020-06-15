package lunaris.streams

import akka.stream.SourceShape
import akka.stream.scaladsl.{GraphDSL, MergeSorted, Source}
import lunaris.genomics.{Locus, LocusOrdering}
import lunaris.recipes.values.{LunValue, RecordStream}
import lunaris.utils.SeqBasedOrdering

object RecordStreamMerger {

  private case class TaggedRecord(record: LunValue.RecordValue, iSource: Int, isLast: Boolean)

  private def getTaggedRecordOrdering(chroms: Seq[String]): Ordering[TaggedRecord] = {
    val chromosomeOrdering = SeqBasedOrdering(chroms)
    new Ordering[TaggedRecord] {
      val locusOrdering: LocusOrdering = new LocusOrdering(chromosomeOrdering)

      override def compare(tr1: TaggedRecord, tr2: TaggedRecord): Int =
        locusOrdering.compare(tr1.record.locus, tr2.record.locus)
    }
  }

  private def taggedStream(source: Source[LunValue.RecordValue, RecordStream.Meta], iSource: Int):
  Source[TaggedRecord, RecordStream.Meta] = {
    source.map(Some(_)).concat(Source.single(None)).sliding(2).map { recordOpts =>
      val isLast = recordOpts.size < 2 || recordOpts(1).isEmpty
      TaggedRecord(recordOpts.head.get, iSource, isLast)
    }
  }

  private class TaggedRecordMerger(nStreams: Int) {
    val locusByStream: Array[Option[Locus]] = Array.fill(nStreams)(None)
    var records: Seq[TaggedRecord] = Seq.empty
    val streamIsExhausted: Array[Boolean] = Array.fill(nStreams)(false)

    private def enterRecord(record: TaggedRecord): Unit = {
      records :+= record
      val iStream = record.iSource
      streamIsExhausted(iStream) = record.isLast
      val locus = record.record.locus
      locusByStream(iStream) = Some(record.record.locus)
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

    private def extractJoinedRecord(): LunValue.RecordValue = {
      val firstRecord = records.head.record
      val (sameRecords, otherRecords) =
        records.partition(record => (record.record.id == firstRecord.id) && (record.record.locus == firstRecord.locus))
      records = otherRecords
      sameRecords.sortBy(_.iSource).map(_.record).reduce((o1, o2) => o1.joinWith(o2).toOption.get)
    }

    def addNext(taggedRecord: TaggedRecord): Seq[LunValue.RecordValue] = {
      enterRecord(taggedRecord)
      val builder = Seq.newBuilder[LunValue.RecordValue]
      while (records.nonEmpty && noStreamHasMoreForLocus(records.head.record.locus)) {
        builder += extractJoinedRecord()
      }
      builder.result()
    }
  }

  def merge(meta: RecordStream.Meta, sources: Seq[Source[LunValue.RecordValue, RecordStream.Meta]]):
  Source[LunValue.RecordValue, RecordStream.Meta] = {
    val taggedSources = sources.zipWithIndex.collect {
      case (source, iSource) => taggedStream(source, iSource)
    }
    val mergedTaggedSource = Source.fromGraph(GraphDSL.create(taggedSources) { implicit builder =>
      sources =>
        import GraphDSL.Implicits._
        if (sources.size == 1) {
          sources.head
        } else {
          val source0 :: source1 :: tail = sources
          implicit val taggedRecordOrdering: Ordering[TaggedRecord] = getTaggedRecordOrdering(meta.chroms)
          var merged = builder.add(new MergeSorted[TaggedRecord]())
          source0.out ~> merged.in0
          source1.out ~> merged.in1
          for (oSource <- tail) {
            val mergedNew = builder.add(new MergeSorted[TaggedRecord]())
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
