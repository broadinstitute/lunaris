package lunaris.app

import org.broadinstitute.yootilz.core.snag.Snag

sealed trait LunarisMode

object LunarisMode {

  case object Batch extends LunarisMode

  case object Server extends LunarisMode

  case object Vep extends LunarisMode

  val all: Set[LunarisMode] = Set(Batch, Server, Vep)

  def parse(string: String): Either[Snag, LunarisMode] = {
    all.find(_.toString == string) match {
      case None =>
        Left(Snag(s"Unknown mode $string. Needs to be either of: ${all.map(_.toString).mkString(", ")}."))
      case Some(mode) =>
        Right(mode)
    }
  }

}
