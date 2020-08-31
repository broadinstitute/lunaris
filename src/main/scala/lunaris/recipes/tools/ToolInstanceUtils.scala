package lunaris.recipes.tools

import lunaris.recipes.eval.LunWorker.RecordStreamWorker
import lunaris.recipes.eval.{LunWorker, WorkerMaker}
import org.broadinstitute.yootilz.core.snag.Snag

object ToolInstanceUtils {

  def newWorkerMakerSingleRef(refKey: String, workers:
  Map[String, LunWorker])(workerGen: RecordStreamWorker => WorkerMaker): Either[Snag, WorkerMaker] = {
    workers.get(refKey) match {
      case Some(fromWorker: RecordStreamWorker) => Right(workerGen(fromWorker))
      case Some(_) => Left(Snag(s"Argument for '$refKey' is not the correct type."))
      case None => Left(Snag(s"No argument provided for $refKey."))
    }
  }

  def newWorkerMakerTwoRefs(
                             refKey1: String, refKey2: String, workers: Map[String, LunWorker]
                           )(
                             workerGen: (RecordStreamWorker, RecordStreamWorker) => WorkerMaker
                           ): Either[Snag, WorkerMaker] = {
    ???
  }

}
