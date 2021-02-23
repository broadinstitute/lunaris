package lunaris.app

import akka.actor.ActorSystem
import akka.stream.Materializer
import lunaris.io.{InputId, ResourceConfig}
import lunaris.io.request.RequestJson
import lunaris.recipes.RecipeChecker
import lunaris.recipes.eval.{LunCompiler, LunRunContext, RunTracker, SnagTracker}
import lunaris.utils.{DebugUtils, IOUtils}

object BatchRunner {
  def run(input: InputId): Unit = {
    val snagOrRunnable = for {
      requestString <-
        input.newReadChannelDisposable().useUp { readChannel =>
          IOUtils.readStringFromChannel(readChannel)
        }
      request <- RequestJson.parse(requestString)
      _ <- RecipeChecker.checkRecipe(request.recipe)
      runnable <- LunCompiler.compile(request)
    } yield runnable
    snagOrRunnable match {
      case Left(snag) =>
        DebugUtils.printSnag("Problem compiling:", snag)
      case Right(runnable) =>
        val actorSystem = ActorSystem("Lunaris")
        val materializer = Materializer(actorSystem)
        val context = LunRunContext(materializer, ResourceConfig.empty)
        val snagTracker = SnagTracker.briefConsolePrinting
        val runTracker = RunTracker(snagTracker)
        val doneFut = runnable.executeAsync(context, runTracker)
        doneFut.onComplete(_ => actorSystem.terminate())(context.materializer.executionContext)
    }
  }
}
