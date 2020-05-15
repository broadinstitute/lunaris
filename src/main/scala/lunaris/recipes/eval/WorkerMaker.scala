package lunaris.recipes.eval

import lunaris.recipes.eval.WorkerMaker.{Receipt, WorkerBox}
import lunaris.recipes.tools
import org.broadinstitute.yootilz.core.snag.Snag

trait WorkerMaker {

  type Tool <: tools.Tool

  def nOrders: Int

  def orderAnotherWorker: Either[Snag, Receipt]

  def finalizeAndShip(): WorkerBox
}

object WorkerMaker {
  case class Receipt(index: Int)

  trait WorkerBox {
    def pickupWorker(receipt: Receipt): LunWorker
    def pickupRunnableOpt(): Option[LunRunnable]
  }
}

