package lunaris.recipes.eval

import lunaris.recipes.eval.WorkerMaker.{Receipt, WorkerBox}
import org.broadinstitute.yootilz.core.snag.Snag

trait WorkerMaker {
  def nOrders: Int

  def orderAnotherWorker: Either[Snag, Receipt]

  def finalizeAndShip(): WorkerBox
}

object WorkerMaker {
  case class Receipt(index: Int)

  trait WorkerBox {
    def pickupWorkerOpt(receipt: Receipt): Option[LunWorker]
    def pickupRunnableOpt(): Option[LunRunnable]
  }
}

