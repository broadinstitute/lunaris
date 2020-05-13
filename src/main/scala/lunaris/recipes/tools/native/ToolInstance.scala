package lunaris.recipes.tools.native

import lunaris.recipes.eval.{LunRunContext, WorkerMaker}

trait ToolInstance {
  def newWorkerMaker(context: LunRunContext): WorkerMaker
}
