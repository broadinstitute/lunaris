package lunaris.app

import com.typesafe.config.{Config, ConfigFactory}
import lunaris.utils.ConfigBox
import lunaris.utils.ConfigBox.{LunarisModeField, StringField}
import org.broadinstitute.yootilz.core.snag.Snag

case class LunarisConfigBox(config: Config) extends ConfigBox[LunarisConfigBox] {
  override def map(mapper: Config => Config): LunarisConfigBox = LunarisConfigBox(mapper(config))

  val mode: LunarisModeField[LunarisConfigBox] = LunarisModeField(this, "lunaris.mode")

  val host: StringField[LunarisConfigBox] = StringField(this, "lunaris.server.host")

  def toServerSettings: Either[Snag, ServerSettings] = {
    for {
      hostVal <- host.get
    } yield ServerSettings(hostVal)
  }

  def toVepServerSettings: Either[Snag, VepServerSettings] = {
    for {
      serverSettings <- toServerSettings
    } yield VepServerSettings(serverSettings)
  }
}

object LunarisConfigBox {
  object DefaultValues {
    val host: String = "localhost"
  }
  def empty: LunarisConfigBox = LunarisConfigBox(ConfigFactory.empty())

  def default: LunarisConfigBox = empty.host.set(DefaultValues.host)
}
