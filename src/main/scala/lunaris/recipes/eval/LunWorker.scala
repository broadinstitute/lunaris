package lunaris.recipes.eval

import lunaris.io.Disposable
import lunaris.io.query.RecordExtractor.HeaderAndRecordEtor
import org.broadinstitute.yootilz.core.snag.Snag

sealed trait LunWorker {

}

object LunWorker {
  trait RecordStreamWorker extends LunWorker {
    def getSnagOrStreamDisposable: Disposable[Either[Snag, HeaderAndRecordEtor]]
  }
}