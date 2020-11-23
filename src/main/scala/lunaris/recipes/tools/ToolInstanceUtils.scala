package lunaris.recipes.tools

import lunaris.recipes.eval.LunWorker.RecordStreamWorker
import lunaris.recipes.eval.{LunWorker, WorkerMaker}
import org.broadinstitute.yootilz.core.snag.Snag

object ToolInstanceUtils {

  private def snagOrWorker(refKey: String, workers: Map[String, LunWorker]): Either[Snag, RecordStreamWorker] = {
    workers.get(refKey) match {
      case Some(fromWorker: RecordStreamWorker) => Right(fromWorker)
      case Some(_) => Left(Snag(s"Worker for '$refKey' is not a stream worker."))
      case None => Left(Snag(s"No worker for $refKey."))
    }
  }

  private def refToWorkerMaker(refKey: String, workers: Map[String, LunWorker]
                              )(
                                workerToSnagOrMaker: RecordStreamWorker => Either[Snag, WorkerMaker]
                              ): Either[Snag, WorkerMaker] = {
    snagOrWorker(refKey, workers).flatMap(workerToSnagOrMaker)
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

  def newWorkerMakerSet(
                         refKeys: Seq[String], workers: Map[String, LunWorker]
                       )(
                         workerGen: Seq[RecordStreamWorker] => WorkerMaker
                       ): Either[Snag, WorkerMaker] = {
    var snagOpt: Option[Snag] = None
    val refKeysIter = refKeys.iterator
    val workersBuilder = Seq.newBuilder[RecordStreamWorker]
    while (snagOpt.isEmpty && refKeysIter.hasNext) {
      val refKey = refKeysIter.next()
      snagOrWorker(refKey, workers) match {
        case Left(snag) => snagOpt = Some(snag)
        case Right(worker) => workersBuilder += worker
      }
    }
    snagOpt match {
      case Some(snag) => Left(snag)
      case None => Right(workerGen(workersBuilder.result()))
    }
  }

  def newWorkerMaker1Set(
                          refKey1: String, refKeys: Seq[String], workers: Map[String, LunWorker]
                        )(
                          workerGen: (RecordStreamWorker, Seq[RecordStreamWorker]) => WorkerMaker
                        ): Either[Snag, WorkerMaker] = {
    ???
  }

}
