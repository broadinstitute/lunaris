package lunaris.app

import lunaris.utils.FiniteNamed

sealed trait LunarisMode

object LunarisMode extends FiniteNamed[LunarisMode] {

  case object Batch extends LunarisMode

  case object Server extends LunarisMode

  case object Vep extends LunarisMode

  case object Misc extends LunarisMode

  override val all: Set[LunarisMode] = Set(Batch, Server, Vep, Misc)

  override val genericName: String = "mode"
}
