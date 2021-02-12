package lunaris.utils

import org.broadinstitute.yootilz.core.snag.Snag

trait FiniteNamed[A] {

  def all: Iterable[A]

  def genericName: String

  def parse(string: String): Either[Snag, A] = {
    all.find(_.toString == string) match {
      case None =>
        Left(Snag(s"Unknown $genericName $string. Needs to be either of: ${all.map(_.toString).mkString(", ")}."))
      case Some(mode) =>
        Right(mode)
    }
  }
}
