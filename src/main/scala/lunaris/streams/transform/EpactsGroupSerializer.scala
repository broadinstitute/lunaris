package lunaris.streams.transform

import akka.stream.scaladsl.Source
import lunaris.recipes.eval.{RunTracker, SnagTracker}
import lunaris.recipes.values.RecordStreamWithMeta
import lunaris.streams.utils.RecordStreamTypes.Record
import lunaris.utils.DedupLilo

import scala.collection.mutable

final class EpactsGroupSerializer(override val groupIdFields: Seq[String])
  extends GroupSerializer {
  private final class Grouper(nBufferedGroupsMax: Int)(snagTracker: SnagTracker) {

    private type VariantIdsBuilder = mutable.Builder[String, Seq[String]]

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
          getGroupId(record) match {
            case Left(snag) =>
              snagTracker.trackSnag(snag)
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

  override def recordsToLines(records: RecordStreamWithMeta, runTracker: RunTracker):
  Source[String, RecordStreamWithMeta.Meta] = {
    val grouper = new Grouper(nBufferedGroupsMax)(runTracker.snagTracker)
    val wrappedSource = records.source.map(Some(_)).concat(Source.single(None))
    wrappedSource.statefulMapConcat(() => grouper.process)
  }
}

object EpactsGroupSerializer {
  object Factory extends GroupSerializer.Factory {
    override val name: String = "EPACTS"
    override def create(groupIdFields: Seq[String]): EpactsGroupSerializer =
      new EpactsGroupSerializer(groupIdFields)
  }
}
