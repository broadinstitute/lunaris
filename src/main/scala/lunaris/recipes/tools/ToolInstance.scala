package lunaris.recipes.tools

import lunaris.recipes.eval.{LunCompileContext, WorkerMaker}
import org.broadinstitute.yootilz.core.snag.Snag

trait ToolInstance {
  def refs: Set[String]
  def newWorkerMaker(context: LunCompileContext, makers: Map[String, WorkerMaker]): Either[Snag, WorkerMaker]
}
