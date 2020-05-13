package lunaris.recipes.eval

import lunaris.recipes.eval.WorkerMaker.{Receipt, WorkerBox}
import org.broadinstitute.yootilz.core.snag.Snag

sealed trait WorkerMaker {
  def orderAnotherWorker: Either[Snag, Receipt]

  def finalizeAndShip(): WorkerBox
}

object WorkerMaker {
  trait Receipt {
    def maker: WorkerMaker
    def index: Int
  }
  trait TypedWorkerMaker[W] extends WorkerMaker {
    def orderAnotherWorker: Either[Snag, Receipt]

    def finalizeAndShip(): TypedWorkerBox[W]
  }
  case class TypedReceipt[W](maker: TypedWorkerMaker[W], index: Int) extends Receipt
  trait WorkerBox {
  }
  trait TypedWorkerBox[W] extends WorkerBox {
    def getWorker(receipt: TypedReceipt[W]): Either[Snag, W]
  }
}
