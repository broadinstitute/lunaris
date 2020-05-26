package lunaris.recipes.values

import lunaris.utils.{Eitherator, SeqBasedOrdering}
import org.broadinstitute.yootilz.core.snag.Snag

case class ObjectStream(meta: ObjectStream.Meta, objects: Eitherator[LunValue.ObjectValue])

object ObjectStream {
  case class Meta(chroms: Seq[String])

  object Meta {
    def sequence(metas: Seq[Meta]): Either[Snag, Meta] = {
      SeqBasedOrdering.combinedSeq(metas.map(_.chroms)).map(Meta(_))
    }
  }
}