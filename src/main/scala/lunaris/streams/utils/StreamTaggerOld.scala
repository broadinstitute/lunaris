package lunaris.streams.utils

import akka.stream.scaladsl.Source

@deprecated("Doesn't work well for empty streams", "2020/11/17")
object StreamTaggerOld {

  @deprecated("Doesn't work well for empty streams", "2020/11/17")
  case class TaggedItemOld[T, S](item: T, sourceId: S, isLast: Boolean)

  @deprecated("Doesn't work well for empty streams", "2020/11/17")
  def tagSource[T, M, S](source: Source[T, M], sourceId: S): Source[TaggedItemOld[T, S], M] = {
    source.map(Some(_)).concat(Source.single(None)).sliding(2).mapConcat { itemOpts =>
      if (itemOpts.size == 2) {
        Seq(TaggedItemOld[T, S](itemOpts.head.get, sourceId, isLast = itemOpts(1).isEmpty))
      } else {
        Seq()
      }
    }
  }

}
