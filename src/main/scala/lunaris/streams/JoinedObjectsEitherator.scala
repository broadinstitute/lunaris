package lunaris.streams

import lunaris.genomics.{Locus, LocusOrdering}
import lunaris.recipes.values.LunValue.RecordValue
import lunaris.utils.{EitherSeqUtils, Eitherator}
import org.broadinstitute.yootilz.core.snag.Snag

class JoinedObjectsEitherator(objectEtors: Seq[Eitherator[RecordValue]],
                              chromOrdering: Ordering[String]) extends Eitherator[RecordValue] {
  val locationOrdering = new LocusOrdering(chromOrdering)
  var snagOpt: Option[Snag] = None
  var allAreExhausted: Boolean = false

  private val etorsWithBuffers = objectEtors.map(new EtorWithBuffer(_))

  def peekNextLocus(): Either[Snag, Option[Locus]] = {
    EitherSeqUtils.traverse(etorsWithBuffers)(_.peekNextLocus()).map(_.flatten).map(_.minOption(locationOrdering))
  }

  class EtorWithBuffer(val objectEtor: Eitherator[RecordValue]) {
    var snagOpt: Option[Snag] = None
    var underlyingIsExhausted: Boolean = false
    var objectsHere: Seq[RecordValue] = Seq.empty
    var objectBeyondOpt: Option[RecordValue] = None

    loadAllHere()

    private def loadAllHere(): Unit = {
      while(snagOpt.isEmpty && !underlyingIsExhausted && objectBeyondOpt.isEmpty) {
        objectEtor.next() match {
          case Left(snag) => snagOpt = Some(snag)
          case Right(Some(nextObject)) =>
            if(objectsHere.isEmpty) {
              objectsHere = Seq(nextObject)
            } else if (objectsHere.head.locus == nextObject.locus) {
              objectsHere :+= nextObject
            } else {
              objectBeyondOpt = Some(nextObject)
            }
          case Right(None) => underlyingIsExhausted = true
        }
      }
    }

    def peekNextObject(): Either[Snag, Option[RecordValue]] = {
      snagOpt match {
        case Some(snag) => Left(snag)
        case None => Right(objectsHere.headOption)
      }
    }

    def peekNextLocus(): Either[Snag, Option[Locus]] = peekNextObject().map(_.map(_.locus))

    def isAtLocus(locus: Locus): Boolean = {
      peekNextObject() match {
        case Right(Some(objectValue)) if objectValue.locus == locus => true
        case _ => false
      }
    }

    def popObjectWithId(id: String): Option[RecordValue] = {
      var resultOpt: Option[RecordValue] = None
      val objectsHereNewBuilder = Seq.newBuilder[RecordValue]
      for(objectHere <- objectsHere) {
        if(objectHere.id == id) {
          resultOpt = Some(objectHere)
        } else {
          objectsHereNewBuilder += objectHere
        }
      }
      objectsHere = objectsHereNewBuilder.result()
      if(objectsHere.isEmpty) {
        objectsHere = objectBeyondOpt.toList
        objectBeyondOpt = None
        loadAllHere()
      }
      resultOpt
    }
  }

  override def next(): Either[Snag, Option[RecordValue]] = {
    snagOpt match {
      case Some(snag) => Left(snag)
      case None =>
        if(allAreExhausted) {
          Right(None)
        } else {
          peekNextLocus() match {
            case Left(snag) =>
              snagOpt = Some(snag)
              Left(snag)
            case Right(Some(locus)) =>
              val nextId = etorsWithBuffers.find(_.isAtLocus(locus)).get.peekNextObject().toOption.get.get.id
              val objects = etorsWithBuffers.flatMap(_.popObjectWithId(nextId))
              var snagOrObjectJoined: Either[Snag, RecordValue] = Right(objects.head)
              for(object2 <- objects.tail) {
                snagOrObjectJoined = snagOrObjectJoined.flatMap(_.joinWith(object2))
              }
              snagOrObjectJoined.map(Some(_))
            case Right(None) =>
              allAreExhausted = true
              Right(None)
          }
        }
    }
  }
}

object JoinedObjectsEitherator {


}
