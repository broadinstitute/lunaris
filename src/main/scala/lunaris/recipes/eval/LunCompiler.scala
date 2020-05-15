package lunaris.recipes.eval

import lunaris.io.request.Request
import lunaris.recipes.eval.WorkerMaker.{Receipt, WorkerBox}
import lunaris.recipes.tools.ToolInstance
import lunaris.utils.EitherSeqUtils
import org.broadinstitute.yootilz.core.snag.Snag

object LunCompiler {
  def compile(request: Request): Either[Snag, LunRunnable] = {
    for {
      instances <- EitherSeqUtils.traverseMap(request.recipe.calls)(_.newInstance)
      context = LunCompileContext.fromRequest(request)
      keysSorted <- sortKeysByDep(instances)
      runnable <- makeRunnable(instances, context, keysSorted)
    } yield runnable
  }

  private def sortKeysByDep(instances: Map[String, ToolInstance]): Either[Snag, Seq[String]] = {
    var snagOpt: Option[Snag] = None
    val builder = Seq.newBuilder[String]
    var unplacedKeys: Set[String] = instances.keySet
    var placedKeys: Set[String] = Set.empty
    while (snagOpt.isEmpty && unplacedKeys.nonEmpty) {
      var couldPlaceAKey: Boolean = false
      val unplacedKeysIter = unplacedKeys.iterator
      while (unplacedKeysIter.hasNext) {
        val unplacedKey = unplacedKeysIter.next()
        if (instances(unplacedKey).refs.values.toSet.subsetOf(placedKeys)) {
          unplacedKeys -= unplacedKey
          placedKeys += unplacedKey
          builder += unplacedKey
          couldPlaceAKey = true
        }
      }
      if (!couldPlaceAKey) {
        snagOpt = Some(Snag(s"There is s circular dependency involving calls ${unplacedKeys.mkString(", ")}."))
      }
    }
    snagOpt match {
      case Some(snag) => Left(snag)
      case None => Right(builder.result())
    }
  }

  private case class Order(receipt: Receipt, sellerKey: String)

  private def makeRunnable(instances: Map[String, ToolInstance],
                           context: LunCompileContext,
                           sortedKeys: Seq[String]): Either[Snag, LunRunnable] = {
    var snagOpt: Option[Snag] = None
    var boxes: Map[String, WorkerBox] = Map.empty
    var orders: Map[String, Map[String, Order]] = Map.empty
    var sortedKeysRemaining: Seq[String] = sortedKeys
    while(snagOpt.isEmpty && sortedKeysRemaining.nonEmpty) {
      val sellerKey = sortedKeysRemaining.head
      sortedKeysRemaining = sortedKeysRemaining.tail
      val workers =
        orders(sellerKey).view.mapValues(order => boxes(order.sellerKey).pickupWorker(order.receipt)).toMap
      instances(sellerKey).newWorkerMaker(context, workers) match {
        case Left(snag) => snagOpt = Some(snag)
        case Right(maker) =>
          for(buyerKey <- sortedKeysRemaining) {
            for((arg,ref) <- instances(buyerKey).refs) {
              if(ref == sellerKey) {
                maker.orderAnotherWorker match {
                  case Left(snag) => snagOpt = Some(snag)
                  case Right(receipt) =>
                    val order = Order(receipt, sellerKey)
                    val ordersForBuyer = orders.getOrElse(buyerKey, Map.empty)
                    val ordersForBuyerNew = ordersForBuyer + (arg -> order)
                    orders += (buyerKey -> ordersForBuyerNew)
                }
              }
            }
          }
          val box = maker.finalizeAndShip()
          boxes += (sellerKey -> box)
      }
    }
    snagOpt match {
      case Some(snag) => Left(snag)
      case None => Right(LunRunnable.combine(boxes.values.flatMap(_.pickupRunnableOpt())))
    }
  }
}
