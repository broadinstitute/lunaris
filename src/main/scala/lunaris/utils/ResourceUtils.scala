package lunaris.utils

import java.io.InputStream

import akka.stream.IOResult
import akka.stream.scaladsl.{Source, StreamConverters}
import akka.util.ByteString
import better.files.Resource

import scala.concurrent.Future

object ResourceUtils {
  def resourceExists(location: String): Boolean = {
    val probeOpt = Resource.asStream(location)
    probeOpt.foreach(_.close())
    probeOpt.nonEmpty
  }

  def resourceAsStreamOpt(location: String): Option[Source[ByteString, Future[IOResult]]] = {
    if (resourceExists(location)) {
      Some(StreamConverters.fromInputStream(() => Resource.getAsStream(location)))
    } else {
      None
    }
  }

  def resourceAsFilteredStreamOpt(location: String)(filter: InputStream => InputStream):
  Option[Source[ByteString, Future[IOResult]]] = {
    if (resourceExists(location)) {
      Some(StreamConverters.fromInputStream(() => filter(Resource.getAsStream(location))))
    } else {
      None
    }
  }
}

