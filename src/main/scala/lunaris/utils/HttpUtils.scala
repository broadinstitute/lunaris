package lunaris.utils

import java.io.InputStream
import akka.actor.ActorSystem
import akka.http.scaladsl.{Http, model}
import akka.http.scaladsl.model.{ContentType, HttpCharsets, HttpEntity, MediaType, MediaTypes, MessageEntity}
import akka.http.scaladsl.server.Route
import akka.stream.Materializer
import akka.stream.scaladsl.Source
import akka.util.ByteString
import io.circe.Json


object HttpUtils {

  object ContentTypes {
    val css: ContentType.WithCharset = MediaTypes.`text/css`.withCharset(HttpCharsets.`UTF-8`)
    val html: ContentType.WithCharset = model.ContentTypes.`text/html(UTF-8)`
    val js: ContentType.WithCharset = MediaTypes.`application/javascript`.withCharset(HttpCharsets.`UTF-8`)
    val tsv: ContentType.WithCharset = MediaTypes.`text/tab-separated-values`.withCharset(HttpCharsets.`UTF-8`)
    val json: ContentType.WithFixedCharset = model.ContentTypes.`application/json`
    val plain: ContentType.WithCharset = model.ContentTypes.`text/plain(UTF-8)`
    val svg: MediaType.Binary = MediaTypes.`image/svg+xml`
  }

  object ResponseBuilder {
    def fromJson(json: Json): HttpEntity.Strict = HttpEntity(HttpUtils.ContentTypes.json, json.spaces2)

    def fromPlainTextString(string: String): HttpEntity.Strict = HttpEntity(HttpUtils.ContentTypes.plain, string)

    def fromHtmlString(string: String): HttpEntity.Strict = HttpEntity(HttpUtils.ContentTypes.html, string)

    def fromTsvByteStream(tsvByteStream: Source[ByteString, _]): HttpEntity.Chunked = {
      HttpEntity(ContentTypes.tsv, tsvByteStream)
    }

    def fromTsvStream(tsvStream: Source[String, _]): HttpEntity.Chunked = {
      HttpEntity(ContentTypes.tsv, tsvStream.map(string => ByteString(string)))
    }

    def fromResourceOpt(contentType: ContentType, location: String): Option[HttpEntity.Chunked] = {
      ResourceUtils.resourceAsStreamOpt(location).map(HttpEntity(contentType, _))
    }

    def fromFilteredResourceOpt(contentType: ContentType, location: String)(filter: InputStream => InputStream):
    Option[HttpEntity.Chunked] = {
      ResourceUtils.resourceAsFilteredStreamOpt(location)(filter).map(HttpEntity(contentType, _))
    }

    def fromResourceOrError(contentType: ContentType, location: String): MessageEntity = {
      fromResourceOpt(contentType, location).getOrElse[MessageEntity](forError(s"Could not load resource at $location."))
    }

    def fromFilteredResourceOrError(contentType: ContentType, location: String)(filter: InputStream => InputStream):
    MessageEntity = {
      fromFilteredResourceOpt(contentType, location)(filter)
        .getOrElse[MessageEntity](forError(s"Could not load resource at $location."))
    }

    def forError(message: String): HttpEntity.Strict = {
      val string = "ERROR: " + message
      HttpEntity(ContentTypes.plain, string)
    }
  }

  def runWebServiceWhileWaiting(route: Route,
                                webInterface: String,
                                port: Int)(waiter: => Unit)(implicit actorSystem: ActorSystem): Unit = {
    println(s"Starting web service at port $port, interface $webInterface.")
    val bindingFuture = Http().bindAndHandle(route, webInterface, port)(Materializer(actorSystem))
    println(s"Web service is now running at port $port, interface $webInterface.")
    println("Press RETURN to stop...")
    waiter
    bindingFuture.flatMap { binding =>
      println("Web service now scheduled to shut down.")
      binding.unbind()
    }(AkkaUtils.getDispatcher(actorSystem))
      .onComplete { _ =>
        actorSystem.terminate()
        println("Web service has been shut down.")
      }(AkkaUtils.getDispatcher(actorSystem))
  }
}
