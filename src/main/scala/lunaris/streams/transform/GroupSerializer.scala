package lunaris.streams.transform

import akka.stream.scaladsl.Source
import lunaris.recipes.eval.RunTracker
import lunaris.recipes.values.RecordStreamWithMeta
import lunaris.streams.utils.RecordStreamTypes.Record
import org.broadinstitute.yootilz.core.snag.Snag

trait GroupSerializer {

  def groupIdFields: Seq[String]

  def isUsefulGroupId(id: String): Boolean = {
    val idTrimmed = id.trim
    idTrimmed.nonEmpty  && idTrimmed != "-"
  }

  protected def getGroupId(record: Record): Either[Snag, String] = {
    var snagOpt: Option[Snag] = None
    var groupIdOpt: Option[String] = None
    val groupIdFieldIter: Iterator[String] = groupIdFields.iterator
    while (groupIdFieldIter.hasNext && snagOpt.isEmpty && groupIdOpt.isEmpty) {
      val groupIdField = groupIdFieldIter.next()
      if (record.has(groupIdField)) {
        record.get(groupIdField).flatMap(_.asString) match {
          case Left(snag) => snagOpt = Some(snag)
          case Right(groupId) =>
            if(isUsefulGroupId(groupId)) {
              groupIdOpt = Some(groupId)
            }
        }
      }
    }
    (snagOpt, groupIdOpt) match {
      case (Some(snag), _) => Left(snag)
      case (_, Some(groupId)) => Right(groupId)
      case _ => Left(Snag(s"Record ${record.id} does not have any of the fields ${groupIdFields.mkString(", ")}."))
    }
  }

  def recordsToLines(recordStream: RecordStreamWithMeta, runTracker: RunTracker):
  Source[String, RecordStreamWithMeta.Meta]
}

object GroupSerializer {

  trait Factory {
    def name: String

    def create(groupIdFields: Seq[String]): GroupSerializer
  }

  object Registry {
    val factories: Set[Factory] = Set(EpactsGroupSerializer.Factory, RareMetalsGroupSerializer.Factory)
    val factoriesByName: Map[String, Factory] = factories.map(factory => (factory.name, factory)).toMap

    def getFactory(name: String): Either[Snag, Factory] = {
      factoriesByName.get(name) match {
        case None => Left(Snag(s"Could not find a serializer '$name'."))
        case Some(factory) => Right(factory)
      }
    }

    def getFactoryOrElse(name: String, defaultFactory: Factory): Factory = {
      factoriesByName.getOrElse(name, defaultFactory)
    }
  }

}