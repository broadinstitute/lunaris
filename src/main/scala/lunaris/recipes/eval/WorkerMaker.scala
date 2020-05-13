package lunaris.recipes.eval

import lunaris.recipes.tools
import org.broadinstitute.yootilz.core.snag.Snag

trait WorkerMaker {

  type Tool <: tools.Tool

  case class Receipt(index: Int)

  trait WorkerBox {
    def pickupWorker(receipt: Receipt): Tool#Worker
  }

  def nOrders: Int

  def orderAnotherWorker: Either[Snag, Receipt]

  def finalizeAndShip(): WorkerBox
}

