package lunaris.streams

import lunaris.genomics.LocusOrdering
import lunaris.recipes.values.LunValue.ObjectValue
import lunaris.utils.Eitherator
import org.broadinstitute.yootilz.core.snag.Snag

class JoinedObjectsEitherator(objectEtors: Seq[Eitherator[ObjectValue]],
                              chromOrdering: Ordering[String]) extends Eitherator[ObjectValue] {
  val locationOrdering = new LocusOrdering(chromOrdering)
  var snagOpt: Option[Snag] = None

  class EtorWithBuffer(val objectEtor: Eitherator[ObjectValue]) {

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
