package lunaris.recipes.eval

import lunaris.recipes.values.RecordStreamWithMeta
import org.broadinstitute.yootilz.core.snag.Snag

sealed trait LunWorker {

}

object LunWorker {

  trait StreamBox {
    def stream: RecordStreamWithMeta

    def snagOrStream: Either[Snag, RecordStreamWithMeta]
  }

  object StreamBox {
    def apply(stream: RecordStreamWithMeta): StreamBox = new StreamBoxForStream(stream)

    def apply(snagOrStream: Either[Snag, RecordStreamWithMeta]): StreamBox =
      new StreamBoxForSnagOrStream(snagOrStream)
  }

  class StreamBoxForStream(val stream: RecordStreamWithMeta) extends StreamBox {
    override def snagOrStream: Either[Snag, RecordStreamWithMeta] = Right(stream)
  }

  class StreamBoxForSnagOrStream(val snagOrStream: Either[Snag, RecordStreamWithMeta]) extends StreamBox {
    override def stream: RecordStreamWithMeta = snagOrStream.toOption.get
  }

  trait RecordStreamWorker extends LunWorker {
    def getStreamBox(context: LunRunContext, runTracker: RunTracker): StreamBox
  }
}