package lunaris.utils

import akka.stream.IOResult
import akka.stream.scaladsl.{Source, StreamConverters}
import akka.util.ByteString
import better.files.Resource

import scala.concurrent.Future

object ResourceUtils {
  def resourceAsStreamOpt(location: String): Option[Source[ByteString, Future[IOResult]]] = {
    val probeOpt = Resource.asStream(location)
    val exists = probeOpt.nonEmpty
    probeOpt.foreach(_.close())
    if(exists) {
      Some(StreamConverters.fromInputStream(() => Resource.getAsStream(location)))
    } else {
      None
    }
  }
}

