package lunaris.streams

import lunaris.recipes.values.LunValue.ObjectValue
import lunaris.streams.JoinedObjectsEitherator.ObjectOrdering
import lunaris.utils.Eitherator
import org.broadinstitute.yootilz.core.snag.Snag

class JoinedObjectsEitherator(objectEtor1: Eitherator[ObjectValue],
                              objectEtor2: Eitherator[ObjectValue]) extends Eitherator[ObjectValue]{
  var nextOpt1: Option[Either[Snag, Option[ObjectValue]]] = None
  var nextOpt2: Option[Either[Snag, Option[ObjectValue]]] = None
  override def next(): Either[Snag, Option[ObjectValue]] = {
        val next1 = nextOpt1 match {
          case Some(next) => next
          case None =>
            val next = objectEtor1.next()
            nextOpt1 = Some(next)
            next
        }
        val next2 = nextOpt2 match {
          case Some(next) => next
          case None =>
            val next = objectEtor1.next()
            nextOpt2 = Some(next)
            next
        }
        (next1, next2) match {
          case (Left(snag), _) => Left(snag)
          case (_, Left(snag)) => Left(snag)
          case (Right(Some(object1)), Right(Some(object2))) =>
            val objectComparison = ObjectOrdering.compare(object1, object2)
            if(objectComparison == 0) {

            }
        }
  }
}

object JoinedObjectsEitherator {
  object ObjectOrdering extends Ordering[ObjectValue] {
    override def compare(object1: ObjectValue, object2: ObjectValue): Int = {
      var comparison: Int = object1.chrom.compare(object2.chrom)
      if(comparison == 0) { comparison =  object1.begin - object2.begin }
      if(comparison == 0) { comparison =  object1.end - object2.end }
      if(comparison == 0) { comparison =  object1.id.compareTo(object2.id) }
      comparison
    }
  }
}
