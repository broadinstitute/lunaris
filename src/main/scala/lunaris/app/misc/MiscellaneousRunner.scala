package lunaris.app.misc

import lunaris.app.LunarisConfigProps
import lunaris.utils.SnagUtils

object MiscellaneousRunner {
  def run(configProps: LunarisConfigProps): Unit = {
    val miscMode = SnagUtils.throwIfSnag(configProps.miscMode.get)
    miscMode match {
      case LunarisMiscellaneousMode.Encrypt =>
        EncryptRunner.run()
      case LunarisMiscellaneousMode.Email =>
        val emailSettings = SnagUtils.throwIfSnag(configProps.toEmailSettings)
        EmailRunner.run(emailSettings)
    }
  }
}
