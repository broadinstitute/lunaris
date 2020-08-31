package lunaris.recipes.tools

import lunaris.recipes.eval.LunWorker.RecordStreamWorker
import lunaris.recipes.eval.{LunWorker, WorkerMaker}
import org.broadinstitute.yootilz.core.snag.Snag

object ToolInstanceUtils {

  private def refToWorkerMaker(refKey: String, workers: Map[String, LunWorker]
                              )(
                                workerToSnagOrMaker: RecordStreamWorker => Either[Snag, WorkerMaker]
                              ): Either[Snag, WorkerMaker] = {
    workers.get(refKey) match {
      case Some(fromWorker: RecordStreamWorker) => workerToSnagOrMaker(fromWorker)
      case Some(_) => Left(Snag(s"Argument for '$refKey' is not the correct type."))
      case None => Left(Snag(s"No argument provided for $refKey."))
    }
  }

  def newWorkerMaker1(
                       refKey: String, workers: Map[String, LunWorker]
                     )(
                       workerGen: RecordStreamWorker => WorkerMaker
                     ): Either[Snag, WorkerMaker] = {
    refToWorkerMaker(refKey, workers) { worker =>
      Right(workerGen(worker))
    }
  }

  def newWorkerMaker2(
                       refKey1: String, refKey2: String, workers: Map[String, LunWorker]
                     )(
                       workerGen: (RecordStreamWorker, RecordStreamWorker) => WorkerMaker
                     ): Either[Snag, WorkerMaker] = {
    refToWorkerMaker(refKey1, workers) { worker1 =>
      refToWorkerMaker(refKey2, workers) { worker2 =>
       Right(workerGen(worker1, worker2))
      }
    }
  }

}
