package lunaris.streams.utils

import akka.stream.scaladsl.Source
import lunaris.recipes.values.{LunValue, RecordStreamWithMeta}
import org.broadinstitute.yootilz.core.snag.Snag

object RecordStreamTypes {

  type Record = LunValue.RecordValue
  type Meta = RecordStreamWithMeta.Meta
  type RecordSource = Source[Record, Meta]
  type SnagLogger = Snag => Unit

}
