package lunaris.app

import lunaris.io.request.RequestJson
import lunaris.io.{InputId, ResourceConfig}
import lunaris.recipes.RecipeChecker
import lunaris.recipes.eval.{LunCompiler, LunRunContext}
import lunaris.utils.{DebugUtils, IOUtils}

object Lunaris {
  def main(args: Array[String]): Unit = {
    if (args.length > 0) {
      val input = InputId(args(0))
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
          val context = LunRunContext(ResourceConfig.empty, LunRunContext.Observer.forLogger(println))
          runnable.execute(context)
      }
    } else {
      println("Error: no input file was provided.")
    }
  }
}
