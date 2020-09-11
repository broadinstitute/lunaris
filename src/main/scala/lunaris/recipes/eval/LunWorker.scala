package lunaris.recipes.eval

import lunaris.io.Disposable
import lunaris.recipes.values.RecordStreamWithMeta
import org.broadinstitute.yootilz.core.snag.Snag

sealed trait LunWorker {

}

object LunWorker {

  trait StreamBox {
    def stream: RecordStreamWithMeta

    def snagOrStream: Either[Snag, RecordStreamWithMeta]

    def snagOrStreamDisposable: Disposable[Either[Snag, RecordStreamWithMeta]]
  }

  object StreamBox {
    def apply(stream: RecordStreamWithMeta): StreamBox = new StreamBoxForStream(stream)

    def apply(snagOrStream: Either[Snag, RecordStreamWithMeta]): StreamBox =
      new StreamBoxForSnagOrStream(snagOrStream)

    def apply(snagOrStreamDisposable: Disposable[Either[Snag, RecordStreamWithMeta]]): StreamBox =
      new StreamBoxForSnagOrStreamDisposable(snagOrStreamDisposable)
  }

  class StreamBoxForStream(val stream: RecordStreamWithMeta) extends StreamBox {
    override def snagOrStream: Either[Snag, RecordStreamWithMeta] = Right(stream)

    override def snagOrStreamDisposable: Disposable[Either[Snag, RecordStreamWithMeta]] =
      Disposable(snagOrStream)(Disposable.Disposer.Noop)
  }

  class StreamBoxForSnagOrStream(val snagOrStream: Either[Snag, RecordStreamWithMeta]) extends StreamBox {
    override def stream: RecordStreamWithMeta = snagOrStream.toOption.get

    override def snagOrStreamDisposable: Disposable[Either[Snag, RecordStreamWithMeta]] =
      Disposable(snagOrStream)(Disposable.Disposer.Noop)
  }

  class StreamBoxForSnagOrStreamDisposable(val snagOrStreamDisposable: Disposable[Either[Snag, RecordStreamWithMeta]])
    extends StreamBox {
    override def stream: RecordStreamWithMeta = snagOrStreamDisposable.a.toOption.get

    override def snagOrStream: Either[Snag, RecordStreamWithMeta] = snagOrStreamDisposable.a
  }

  trait RecordStreamWorker extends LunWorker {
    def getStreamBox(context: LunRunContext): StreamBox
  }

}