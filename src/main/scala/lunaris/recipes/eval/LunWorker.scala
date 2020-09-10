package lunaris.recipes.eval

import lunaris.io.Disposable
import lunaris.recipes.values.RecordStreamWithMeta
import org.broadinstitute.yootilz.core.snag.Snag

sealed trait LunWorker {

}

object LunWorker {

  trait StreamBox {
    def stream: RecordStreamWithMeta

    def snagOrStreamDisposable: Disposable[Either[Snag, RecordStreamWithMeta]]
  }

  object StreamBox {
    def apply(stream: RecordStreamWithMeta): StreamBox = new StreamBoxForStream(stream)

    def apply(snagOrStreamDisposable: Disposable[Either[Snag, RecordStreamWithMeta]]): StreamBox =
      new StreamBoxForSnagOrStreamDisposable(snagOrStreamDisposable)
  }

  class StreamBoxForStream(val stream: RecordStreamWithMeta) extends StreamBox {
    override def snagOrStreamDisposable: Disposable[Either[Snag, RecordStreamWithMeta]] =
      Disposable(Right(stream))(Disposable.Disposer.Noop)
  }

  class StreamBoxForSnagOrStreamDisposable(val snagOrStreamDisposable: Disposable[Either[Snag, RecordStreamWithMeta]])
    extends StreamBox {
    override def stream: RecordStreamWithMeta = snagOrStreamDisposable.a.toOption.get
  }

  trait RecordStreamWorker extends LunWorker {
    def getStreamBox(context: LunRunContext): StreamBox

    def getStream(context: LunRunContext): RecordStreamWithMeta = getStreamBox(context).stream
  }

}