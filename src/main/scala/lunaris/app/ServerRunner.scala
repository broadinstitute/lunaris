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

object ServerRunner {

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
        path("lunaris" / "lunaris.html") {
          get {
            complete(
              HttpUtils.ResponseBuilder.fromResourceOrError(ContentTypes.`text/html(UTF-8)`, "web/lunaris.html")
            )
          }
        },
        path("lunaris" / "css" / "lunaris.css") {
          get {
            complete(
              HttpUtils.ResponseBuilder.fromResourceOrError(HttpUtils.ContentTypes.css, "web/css/lunaris.css")
            )
          }
        },
        path("lunaris" / "js" / "lunaris.js") {
          get {
            complete(
              HttpUtils.ResponseBuilder.fromResourceOrError(HttpUtils.ContentTypes.js, "web/js/lunaris.js")
            )
          }
        },
        path("lunaris" / "query") {
          respondWithHeader(`Access-Control-Allow-Origin`.*) {
            post {
              decodeRequest {
                entity(as[String]) { requestString =>
                  val snagOrRunnable = for {
                    request <- RequestJson.parse(requestString)
                    _ <- RecipeChecker.checkRecipe(request.recipe)
                    runnable <- LunCompiler.compile(request)
                  } yield runnable
                  snagOrRunnable match {
                    case Left(snag) => complete(HttpUtils.ResponseBuilder.forError(snag.report))
                    case Right(runnable) =>
                      val runContext =
                        LunRunContext(materializer, ResourceConfig.empty, LunRunContext.Observer.forLogger(println))
                      runnable.getStream(runContext).a match {
                        case Left(snag) => complete(HttpUtils.ResponseBuilder.forError(snag.report))
                        case Right(recordStream) =>
                          complete(HttpUtils.ResponseBuilder.fromTsvStream(recordStream.recover { ex =>
                            val report = Snag(ex).report
                            println(report)
                            report
                          }))
                      }
                  }
                }
              }
            }
          }
        },
        pathPrefix("lunaris" / "requests" / Remaining) { requestFile =>
          get { httpRequestContext =>
            val paramsMap = httpRequestContext.request.uri.query().toMap
            val inputStreamFilter = ParamsReplacer.getInputStreamFilter(paramsMap)
            httpRequestContext.complete(
              HttpUtils.ResponseBuilder
                .fromFilteredResourceOrError(HttpUtils.ContentTypes.json,
                  "web/requests/" + requestFile)(inputStreamFilter)
            )
          }
        }
      )
    HttpUtils.runWebServiceWhileWaiting(route, host, port)(StdIn.readLine())
  }
}
