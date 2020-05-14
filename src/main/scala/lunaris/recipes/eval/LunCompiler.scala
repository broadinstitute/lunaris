package lunaris.recipes.eval

import lunaris.io.request.Request
import lunaris.recipes.eval.WorkerMaker.{Receipt, WorkerBox}
import lunaris.recipes.tools.{ToolCall, ToolInstance}
import lunaris.utils.EitherSeqUtils
import org.broadinstitute.yootilz.core.snag.Snag

object LunCompiler {
  def compile(request: Request): Either[Snag, LunRunnable] = {
    for {
      instances <- EitherSeqUtils.traverseMap(request.recipe.calls)(_.newInstance)
      context = LunCompileContext.fromRequest(request)
      keysSorted <- sortKeysByDep(instances)
      makers <- makeMakers(instances, keysSorted, context)
      receipts <- orderEffectiveFinals(request.recipe.calls, makers)
      boxes = getWorkerBoxes(keysSorted, makers)
    } yield createRunnable(receipts, boxes)
  }

  private def sortKeysByDep(instances: Map[String, ToolInstance]): Either[Snag, Seq[String]] = {
    var snagOpt: Option[Snag] = None
    val builder = Seq.newBuilder[String]
    var unplacedKeys: Set[String] = instances.keySet
    var placedKeys: Set[String] = Set.empty
    while(snagOpt.isEmpty && unplacedKeys.nonEmpty) {
      var couldPlaceAKey: Boolean =  false
      val unplacedKeysIter = unplacedKeys.iterator
      while(unplacedKeysIter.hasNext) {
        val unplacedKey = unplacedKeysIter.next()
        if(instances(unplacedKey).refs.subsetOf(placedKeys)) {
          unplacedKeys -= unplacedKey
          placedKeys += unplacedKey
          builder += unplacedKey
          couldPlaceAKey = true
        }
      }
      if(!couldPlaceAKey) {
        snagOpt = Some(Snag(s"There is s circular dependency involving calls ${unplacedKeys.mkString(", ")}."))
      }
    }
    snagOpt match {
      case Some(snag) => Left(snag)
      case None => Right(builder.result())
    }
  }

  private def makeMakers(instances: Map[String, ToolInstance],
                         sortedKeys: Seq[String],
                         context: LunCompileContext): Either[Snag, Map[String, WorkerMaker]] = {
    var snagOpt: Option[Snag] = None
    var makers: Map[String, WorkerMaker] = Map.empty
    val keyIter = sortedKeys.iterator
    while(snagOpt.isEmpty && keyIter.hasNext) {
      val key = keyIter.next()
      val instance = instances(key)
      instance.newWorkerMaker(context, makers) match {
        case Left(snag) => snagOpt = Some(snag)
        case Right(maker) => makers += (key -> maker)
      }
    }
    snagOpt match {
      case Some(snag) => Left(snag)
      case None => Right(makers)
    }
  }

  private def orderEffectiveFinals(calls: Map[String, ToolCall],
                                   makers: Map[String, WorkerMaker]): Either[Snag, Map[String, WorkerMaker.Receipt]] = {
    var snagOpt: Option[Snag] = None
    val builder = Map.newBuilder[String, WorkerMaker.Receipt]
    for((key, maker) <- makers) {
      if(maker.nOrders == 0 && calls(key).tool.hasEffect) {
        maker.orderAnotherWorker match {
          case Left(snag) => snagOpt = Some(snag)
          case Right(receipt) => builder += (key -> receipt)
        }
      }
    }
    snagOpt match {
      case Some(snag) => Left(snag)
      case None => Right(builder.result())
    }
  }

  private def getWorkerBoxes(sortedKeys: Seq[String], makers: Map[String, WorkerMaker]): Map[String, WorkerBox] = {
    val builder = Map.newBuilder[String, WorkerBox]
    for(key <- sortedKeys) {
      builder += (key -> makers(key).finalizeAndShip())
    }
    builder.result()
  }

  private def createRunnable(receipts: Map[String, WorkerMaker.Receipt],
                             boxes: Map[String, WorkerBox]): LunRunnable = {
    val runnables = receipts.collect {
      case (key, receipt) => boxes(key).pickupWorkerAsRunnable(receipt)
    }
    LunRunnable.combine(runnables)
  }
}
