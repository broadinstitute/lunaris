package lunaris.utils

import akka.actor.ActorSystem
import akka.dispatch.Dispatchers

import scala.concurrent.ExecutionContextExecutor

object AkkaUtils {
  def getDispatcher(actorSystem: ActorSystem): ExecutionContextExecutor = getBlockingDispatcher(actorSystem)

  def getDefaultDispatcher(actorSystem: ActorSystem): ExecutionContextExecutor = actorSystem.dispatcher

  def getBlockingDispatcher(actorSystem: ActorSystem): ExecutionContextExecutor =
    actorSystem.dispatchers.lookup(Dispatchers.DefaultBlockingDispatcherId)
}
