package lunaris.streams.utils

import akka.stream.scaladsl.Source

object StreamTagger {

  case class TaggedItem[T, S](item: T, sourceId: S, isLast: Boolean)

  def tagSource[T, M, S](source: Source[T, M], sourceId: S): Source[TaggedItem[T, S], M] = {
    source.map(Some(_)).concat(Source.single(None)).sliding(2).map { itemOpts =>
      val isLast = itemOpts.size < 2 || itemOpts(1).isEmpty
      TaggedItem[T, S](itemOpts.head.get, sourceId, isLast)
    }
  }

}
