package lunaris.recipes.eval

import lunaris.io.{Disposable, ResourceConfig}
import lunaris.recipes.values.RecordStreamOld
import org.broadinstitute.yootilz.core.snag.Snag

sealed trait LunWorker {

}

object LunWorker {
  trait ObjectStreamWorker extends LunWorker {
    def getSnagOrStreamDisposable(resourceConfig: ResourceConfig): Disposable[Either[Snag, RecordStreamOld]]
  }
}