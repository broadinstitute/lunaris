package lunaris.streams

import akka.stream.scaladsl.Source
import lunaris.genomics.Locus
import lunaris.recipes.values.RecordStreamWithMeta
import lunaris.streams.utils.RecordStreamTypes.Record
import org.broadinstitute.yootilz.core.snag.Snag

import scala.collection.mutable

object GeneGroupGenerator {
  type SnagLogger = Snag => Unit

  class Grouper(field: String)(snagLogger: SnagLogger) {

    type VariantIdsBuilder = mutable.Builder[String, Seq[String]]

    class GroupBuffer(val groupId: String, val variantIdsBuilder: VariantIdsBuilder, var lastLocus: Locus) {
      def add(record: Record): Unit = {
        variantIdsBuilder += record.id
        lastLocus = record.locus
      }
    }

    object GroupBuffer {
      def create(groupId: String, record: Record): GroupBuffer = {
        val variantIdsBuilder = Seq.newBuilder[String]
        variantIdsBuilder += record.id
        new GroupBuffer(groupId, variantIdsBuilder, record.locus)
      }
    }

    var groupBuffers: Map[String, GroupBuffer] = Map.empty

    def process(recordOpt: Option[Record]): Seq[String] = {
      ???
    }
  }

  def generateGroups(records: RecordStreamWithMeta, field: String, nBufferedGroupsMax: Int)(snagLogger: SnagLogger):
  Source[String, RecordStreamWithMeta.Meta] = {
    val grouper = new Grouper(field)(snagLogger)
    val wrappedSource = records.source.map(Some(_)).concat(Source.single(None))
    wrappedSource.statefulMapConcat(() => grouper.process)
  }

}
