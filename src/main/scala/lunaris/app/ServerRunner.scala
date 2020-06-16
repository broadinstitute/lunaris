package lunaris.app

import java.nio.charset.StandardCharsets

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.{ContentTypes, HttpEntity}
import akka.http.scaladsl.server.Directives.{complete, get, path}
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.Source
import akka.util.ByteString

import scala.concurrent.ExecutionContextExecutor
import scala.io.StdIn
import scala.io.{Source => ScalaSource}

object ServerRunner {
  val host: String = "localhost"
  val port: Int = 8080

  def run(): Unit = {
    println(s"Starting web service at http://$host:$port")
    implicit val system: ActorSystem = ActorSystem("my-system")
    implicit val materializer: ActorMaterializer = ActorMaterializer()
    implicit val executionContext: ExecutionContextExecutor = system.dispatcher
    val route =
      path("lunaris") {
        get {
          complete(
            HttpEntity(
              ContentTypes.`text/html(UTF-8)`,
              Source
                .fromIterator(() => ScalaSource.fromResource("web/html/index.html").getLines())
                .map(string => ByteString(string, StandardCharsets.UTF_8))
            )
          )
        }
      }
    val bindingFuture = Http().bindAndHandle(route, "localhost", 8080)
    println(s"Web service is now running at http://$host:$port/.")
    println("Press RETURN to stop...")
    StdIn.readLine() // let it run until user presses return
    bindingFuture.flatMap { binding =>
      println("Scheduled shutdown of web service.")
      binding.unbind()
    }
      .onComplete(_ => system.terminate())
    println("Done!")
  }
}
