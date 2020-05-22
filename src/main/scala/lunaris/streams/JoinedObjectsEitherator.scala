package lunaris.streams

import lunaris.genomics.LocusOrdering
import lunaris.recipes.values.LunValue.ObjectValue
import lunaris.utils.Eitherator
import org.broadinstitute.yootilz.core.snag.Snag

class JoinedObjectsEitherator(objectEtors: Seq[Eitherator[ObjectValue]],
                              chromOrdering: Ordering[String]) extends Eitherator[ObjectValue] {
  val locationOrdering = new LocusOrdering(chromOrdering)
  var snagOpt: Option[Snag] = None

  private val etorsWithBuffers = objectEtors.map(new EtorWithBuffer(_))

  class EtorWithBuffer(val objectEtor: Eitherator[ObjectValue]) {
    var snagOpt: Option[Snag] = None
    var underlyingIsExhausted: Boolean = false
    var objectsHere: Seq[ObjectValue] = Seq.empty
    var objectBeyondOpt: Option[ObjectValue] = None

    def loadAllHere(): Unit = {
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
  }



  override def next(): Either[Snag, Option[ObjectValue]] = {
    snagOpt match {
      case Some(snag) => Left(snag)
      case None =>

        ???
    }
  }
}

object JoinedObjectsEitherator {


}
