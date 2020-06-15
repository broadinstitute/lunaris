package lunaris.recipes.tools

import lunaris.recipes.eval.{LunCompileContext, LunWorker, WorkerMaker}
import org.broadinstitute.yootilz.core.snag.Snag

trait ToolInstance {
  def refs: Map[String, String]
  def newWorkerMaker(context: LunCompileContext, workers: Map[String, LunWorker]): Either[Snag, WorkerMaker]
}


