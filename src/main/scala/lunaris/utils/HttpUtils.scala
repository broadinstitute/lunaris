package lunaris.utils

import akka.http.scaladsl.model.{ContentType, ContentTypes, HttpEntity, MessageEntity, UniversalEntity}


object HttpUtils {

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
    HttpEntity(ContentTypes.`text/html(UTF-8)`, string)
  }
}
