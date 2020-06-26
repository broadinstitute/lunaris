package lunaris.app

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.ContentTypes
import akka.http.scaladsl.model.headers.`Access-Control-Allow-Origin`
import akka.http.scaladsl.server.Directives.{complete, get, path, _}
import akka.stream.Materializer
import lunaris.io.ResourceConfig
import lunaris.io.request.RequestJson
import lunaris.recipes.RecipeChecker
import lunaris.recipes.eval.{LunCompiler, LunRunContext}
import lunaris.utils.HttpUtils
import org.broadinstitute.yootilz.core.snag.Snag

import scala.concurrent.ExecutionContextExecutor
import scala.io.StdIn

object ServerRunner {

  object Defaults {
    val host: String = "localhost"
    val port: Int = 8080
  }

  def run(hostOpt: Option[String], portOpt: Option[Int]): Unit = {
    val host = hostOpt.getOrElse(Defaults.host)
    val port = portOpt.getOrElse(Defaults.port)
    println(s"Starting web service at http://$host:$port")
    implicit val actorSystem: ActorSystem = ActorSystem("Lunaris-Actor-System")
    implicit val materializer: Materializer = Materializer(actorSystem)
    implicit val executionContext: ExecutionContextExecutor = actorSystem.dispatcher
    val route =
      concat(
        path("lunaris" / "lunaris.html") {
          get {
            complete(
              HttpUtils.fromResourceOrError(ContentTypes.`text/html(UTF-8)`, "web/lunaris.html")
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
        path("lunaris" / "js" / "lunaris.js") {
          get {
            complete(
              HttpUtils.fromResourceOrError(HttpUtils.ContentTypes.css, "web/js/lunaris.js")
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
                    case Left(snag) => complete(HttpUtils.forError(snag.report))
                    case Right(runnable) =>
                      val runContext =
                        LunRunContext(materializer, ResourceConfig.empty, LunRunContext.Observer.forLogger(println))
                      runnable.getStream(runContext).a match {
                        case Left(snag) => complete(HttpUtils.forError(snag.report))
                        case Right(recordStream) =>
                          complete(HttpUtils.fromTsvStream(recordStream.recover { ex =>
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
          get {
            complete(
              HttpUtils.fromResourceOrError(HttpUtils.ContentTypes.json, "web/requests/" + requestFile)
            )
          }
        }
      )
    val bindingFuture = Http().bindAndHandle(route, host, port)
    println(s"Web service is now running at http://$host:$port/.")
    println("Press RETURN to stop...")
    StdIn.readLine() // let it run until user presses return
    bindingFuture.flatMap { binding =>
      println("Scheduled shutdown of web service.")
      binding.unbind()
    }
      .onComplete(_ => actorSystem.terminate())
    println("Done!")
  }
}
