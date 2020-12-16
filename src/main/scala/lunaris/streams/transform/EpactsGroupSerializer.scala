package lunaris.streams.transform

import akka.stream.scaladsl.Source
import lunaris.recipes.values.RecordStreamWithMeta
import lunaris.streams.utils.RecordStreamTypes.{Record, SnagLogger}
import lunaris.utils.DedupLilo

import scala.collection.mutable

final class EpactsGroupSerializer(snagLogger: SnagLogger) {
  private final class Grouper(field: String, nBufferedGroupsMax: Int)(snagLogger: SnagLogger) {

    private type VariantIdsBuilder = mutable.Builder[String, Seq[String]]

    private final class GroupBuffer(val groupId: String, val variantIdsBuilder: VariantIdsBuilder) {
      def add(record: Record): Unit = {
        variantIdsBuilder += record.id
      }
    }

    private var variantIdBuilders: Map[String, VariantIdsBuilder] = Map.empty

    private val groupIdStack = DedupLilo[String](nBufferedGroupsMax)

    private def toLine(groupId: String, variantIds: Seq[String]): String = groupId + "\t" + variantIds.mkString(" ")

    private def flush(groupId: String): String = {
      val variantIds = variantIdBuilders(groupId).result()
      variantIdBuilders -= groupId
      toLine(groupId, variantIds)
    }

    def process(recordOpt: Option[Record]): Seq[String] = {
      recordOpt match {
        case Some(record) =>
          record.get(field).flatMap(_.asString) match {
            case Left(snag) =>
              snagLogger(snag)
              Seq.empty
            case Right(groupId) =>
              val variantIdBuilder = variantIdBuilders.getOrElse(groupId, {
                val variantIdBuilderNew = Seq.newBuilder[String]
                variantIdBuilders += (groupId -> variantIdBuilderNew)
                variantIdBuilderNew
              })
              variantIdBuilder += record.id
              groupIdStack.add(groupId) match {
                case DedupLilo.Result.ExistingItem =>
                  Seq.empty
                case DedupLilo.Result.NewItem =>
                  Seq.empty
                case DedupLilo.Result.Overflow(groupIdFinished) =>
                  Seq(flush(groupIdFinished))
              }
          }
        case None =>
          groupIdStack.items.reverse.map(flush)
      }
    }
  }

  val nBufferedGroupsMax: Int = 10

  def generateGroups(records: RecordStreamWithMeta, field: String):
  Source[String, RecordStreamWithMeta.Meta] = {
    val grouper = new Grouper(field, nBufferedGroupsMax)(snagLogger)
    val wrappedSource = records.source.map(Some(_)).concat(Source.single(None))
    wrappedSource.statefulMapConcat(() => grouper.process)
  }

}
