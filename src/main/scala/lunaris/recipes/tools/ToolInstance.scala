package lunaris.recipes.tools

import lunaris.recipes.eval.{LunRunContext, WorkerMaker}

trait ToolInstance {
  def newWorkerMaker(context: LunRunContext): WorkerMaker
}
