package lunaris.recipes.eval

import lunaris.io.Disposable
import lunaris.recipes.values.RecordStream
import org.broadinstitute.yootilz.core.snag.Snag

sealed trait LunWorker {

}

object LunWorker {
  trait RecordStreamWorker extends LunWorker {
    def getSnagOrStreamDisposable(context: LunRunContext): Disposable[Either[Snag, RecordStream]]
  }
}