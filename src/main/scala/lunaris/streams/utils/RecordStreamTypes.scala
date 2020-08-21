package lunaris.streams.utils

import akka.stream.scaladsl.Source
import lunaris.recipes.values.{LunValue, RecordStreamWithMeta}

object RecordStreamTypes {

  type Record = LunValue.RecordValue
  type Meta = RecordStreamWithMeta.Meta
  type RecordSource = Source[Record, Meta]

}
