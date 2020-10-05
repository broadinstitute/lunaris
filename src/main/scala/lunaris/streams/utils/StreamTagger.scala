package lunaris.streams.utils

import akka.stream.scaladsl.Source

object StreamTagger {

  case class TaggedItem[T, S](item: T, sourceId: S, isLast: Boolean)

  def tagSource[T, M, S](source: Source[T, M], sourceId: S): Source[TaggedItem[T, S], M] = {
    source.map(Some(_)).concat(Source.single(None)).sliding(2).mapConcat { itemOpts =>
      if (itemOpts.size == 2) {
        Seq(TaggedItem[T, S](itemOpts.head.get, sourceId, isLast = itemOpts(1).isEmpty))
      } else {
        Seq()
      }
    }
  }

}
