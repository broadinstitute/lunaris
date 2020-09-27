package lunaris.utils

import org.broadinstitute.yootilz.core.snag.Snag

import scala.util.control.NonFatal

object SnagUtils {

  def tryOrSnag[T](gen: => T): Either[Snag, T] = {
    try {
      Right(gen)
    } catch {
      case NonFatal(exception) => Left(Snag(exception))
    }
  }

}
