package lunaris.app

import akka.actor.ActorSystem
import akka.http.scaladsl.model.ContentTypes
import akka.http.scaladsl.model.headers.`Access-Control-Allow-Origin`
import akka.http.scaladsl.server.Directives.{complete, get, path, _}
import akka.http.scaladsl.server.Route
import akka.stream.Materializer
import lunaris.io.ResourceConfig
import lunaris.io.request.RequestJson
import lunaris.io.request.examples.ParamsReplacer
import lunaris.recipes.RecipeChecker
import lunaris.recipes.eval.{LunCompiler, LunRunContext}
import lunaris.utils.HttpUtils
import org.broadinstitute.yootilz.core.snag.Snag

import scala.io.StdIn

object VariantEffectPredictorServerRunner {

  object Defaults {
    val host: String = "localhost"
    val port: Int = 8080
  }

  def run(hostOpt: Option[String], portOpt: Option[Int]): Unit = {
    val host = hostOpt.getOrElse(Defaults.host)
    val port = portOpt.getOrElse(Defaults.port)
    implicit val actorSystem: ActorSystem = ActorSystem("Lunaris-Actor-System")
    implicit val materializer: Materializer = Materializer(actorSystem)
    val route: Route =
      concat(
        path("lunaris" / "predictor.html") {
          get {
            complete(
              HttpUtils.fromResourceOrError(ContentTypes.`text/html(UTF-8)`, "web/predictor.html")
            )
          }
        },
        path("lunaris" / "css" / "lunaris.css") {
          get {
            complete(
              HttpUtils.fromResourceOrError(HttpUtils.ContentTypes.css, "web/css/lunaris.css")
            )
          }
        },
        path("lunaris" / "js" / "predictor.js") {
          get {
            complete(
              HttpUtils.fromResourceOrError(HttpUtils.ContentTypes.js, "web/js/predictor.js")
            )
          }
        }
      )
    HttpUtils.runWebServiceWhileWaiting(route, host, port)(StdIn.readLine())
  }
}
