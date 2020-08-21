package lunaris.recipes.values

import akka.stream.scaladsl.Source
import lunaris.recipes.values.LunType.RecordType
import lunaris.recipes.values.RecordStreamWithMeta.Meta
import lunaris.utils.SeqBasedOrdering
import org.broadinstitute.yootilz.core.snag.Snag

class RecordStreamWithMeta(val meta: Meta, val source: Source[LunValue.RecordValue, Meta]) {
}

object RecordStreamWithMeta {
  def apply(meta: Meta, source: Source[LunValue.RecordValue, Meta]): RecordStreamWithMeta =
    new RecordStreamWithMeta(meta, source)

  case class Meta(objectType: RecordType, chroms: Seq[String])

  object Meta {
    def sequence(metas: Seq[Meta]): Either[Snag, Meta] = {
      if(metas.isEmpty) {
        Left(Snag("Need at least one meta information."))
      } else {
        var objectTypeCombined: RecordType = metas.head.objectType
        for(meta <- metas.tail) {
          objectTypeCombined = objectTypeCombined.joinWith(meta.objectType)
        }
        SeqBasedOrdering.combinedSeq(metas.map(_.chroms)).map(Meta(objectTypeCombined, _))
      }
    }
  }
}


