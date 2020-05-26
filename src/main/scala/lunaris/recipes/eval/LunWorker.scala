package lunaris.recipes.eval

import lunaris.io.query.RecordExtractor.HeaderAndRecordEtor
import lunaris.io.{Disposable, ResourceConfig}
import lunaris.recipes.values.ObjectStream
import org.broadinstitute.yootilz.core.snag.Snag

sealed trait LunWorker {

}

object LunWorker {
  trait RecordStreamWorker extends LunWorker {
    def getSnagOrStreamDisposable(resourceConfig: ResourceConfig): Disposable[Either[Snag, HeaderAndRecordEtor]]
  }
  trait ObjectStreamWorker extends LunWorker {
    def getSnagOrStreamDisposable(resourceConfig: ResourceConfig): Disposable[Either[Snag, ObjectStream]]
  }
}