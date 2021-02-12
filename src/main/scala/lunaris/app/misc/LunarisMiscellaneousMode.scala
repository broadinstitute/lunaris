package lunaris.app.misc

import lunaris.utils.FiniteNamed

sealed trait LunarisMiscellaneousMode

object LunarisMiscellaneousMode extends FiniteNamed[LunarisMiscellaneousMode] {

  case object Encrypt extends LunarisMiscellaneousMode

  case object Email extends LunarisMiscellaneousMode

  override val all: Set[LunarisMiscellaneousMode] = Set(Encrypt, Email)

  override val genericName: String = "misc mode"
}
