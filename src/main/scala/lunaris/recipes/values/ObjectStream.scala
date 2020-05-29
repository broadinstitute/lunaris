package lunaris.recipes.values

import lunaris.recipes.values.LunType.ObjectType
import lunaris.utils.{Eitherator, SeqBasedOrdering}
import org.broadinstitute.yootilz.core.snag.Snag

case class ObjectStream(meta: ObjectStream.Meta, objects: Eitherator[LunValue.ObjectValue])

object ObjectStream {
  case class Meta(objectType: ObjectType, chroms: Seq[String])

  object Meta {
    def sequence(metas: Seq[Meta]): Either[Snag, Meta] = {
      if(metas.isEmpty) {
        Left(Snag("Need at least one meta information."))
      } else {
        var objectTypeCombined: ObjectType = metas.head.objectType
        for(meta <- metas.tail) {
          objectTypeCombined = objectTypeCombined.joinWith(meta.objectType)
        }
        SeqBasedOrdering.combinedSeq(metas.map(_.chroms)).map(Meta(objectTypeCombined, _))
      }
    }
  }
}