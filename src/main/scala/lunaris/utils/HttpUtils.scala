package lunaris.utils

import akka.http.scaladsl.model
import akka.http.scaladsl.model.{ContentType, HttpCharsets, HttpEntity, MediaType, MediaTypes, MessageEntity}
import akka.stream.scaladsl.Source
import akka.util.ByteString


object HttpUtils {

  object ContentTypes {
    val css: ContentType.WithCharset = MediaTypes.`text/css`.withCharset(HttpCharsets.`UTF-8`)
    val html: ContentType.WithCharset = model.ContentTypes.`text/html(UTF-8)`
    val js: MediaType.WithOpenCharset = MediaTypes.`application/javascript`
    val tsv: ContentType.WithCharset = MediaTypes.`text/tab-separated-values`.withCharset(HttpCharsets.`UTF-8`)
    val json: ContentType.WithFixedCharset = model.ContentTypes.`application/json`
  }

  def fromTsvStream(tsvStream: Source[String, _]): HttpEntity.Chunked = {
    HttpEntity(ContentTypes.tsv, tsvStream.map(string => ByteString(string)))
  }

  def fromResourceOpt(contentType: ContentType, location: String): Option[HttpEntity.Chunked] = {
    ResourceUtils.resourceAsStreamOpt(location).map(HttpEntity(contentType, _))
  }

  def fromResourceOrError(contentType: ContentType, location: String): MessageEntity = {
    fromResourceOpt(contentType, location).getOrElse[MessageEntity](forError(s"Could not load resource at $location."))
  }

  def forError(message: String): HttpEntity.Strict = {
    val string =
      s"""
        |<html>
        |<head>
        |<title>Error: $message</title>
        |</head>
        |<body>
        |<h1>Error!</h1>
        |<p>Error: $message</p>
        |</body>
        |</html>""".stripMargin
    HttpEntity(ContentTypes.html, string)
  }
}
