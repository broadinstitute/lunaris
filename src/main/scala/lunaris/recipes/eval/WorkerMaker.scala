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

  trait WithOutput extends WorkerMaker {
    private var nOrdersField: Int = 0

    override def nOrders: Int = nOrdersField

    override def orderAnotherWorker: Either[Snag, WorkerMaker.Receipt] = {
      if (nOrdersField == 0) {
        nOrdersField = 1
        Right(WorkerMaker.Receipt(0))
      } else {
        Left(Snag(s"Multiplication of streams is not supported at this time."))
      }
    }
  }
}

