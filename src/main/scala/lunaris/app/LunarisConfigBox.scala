package lunaris.app

import com.typesafe.config.{Config, ConfigFactory}
import lunaris.utils.ConfigBox
import lunaris.utils.ConfigBox.{FileField, IntField, LunarisModeField, StringField}
import org.broadinstitute.yootilz.core.snag.Snag

case class LunarisConfigBox(config: Config) extends ConfigBox[LunarisConfigBox] {
  override def map(mapper: Config => Config): LunarisConfigBox = LunarisConfigBox(mapper(config))

  val mode: LunarisModeField[LunarisConfigBox] = LunarisModeField(this, "lunaris.mode")
  val host: StringField[LunarisConfigBox] = StringField(this, "lunaris.server.host")
  val port: IntField[LunarisConfigBox] = IntField(this, "lunaris.server.port")
  val inputsFolder: FileField[LunarisConfigBox] = FileField(this, "lunaris.vep.inputsFolder")
  val resultsFolder: FileField[LunarisConfigBox] = FileField(this, "lunaris.vep.resultsFolder")

  def toServerSettings: Either[Snag, ServerSettings] = {
    for {
      hostVal <- host.get
      portVal <- port.get
    } yield ServerSettings(hostVal, portVal)
  }

  def toVepSettings: Either[Snag, VepSettings] = {
    for {
      inputsFolderVal <- inputsFolder.get
      resultsFolderVal <- resultsFolder.get
    } yield VepSettings(inputsFolderVal, resultsFolderVal)
  }

  def toVepServerSettings: Either[Snag, VepServerSettings] = {
    for {
      serverSettings <- toServerSettings
      vepSettings <- toVepSettings
    } yield VepServerSettings(serverSettings, vepSettings)
  }
}

object LunarisConfigBox {
  object DefaultValues {
    val host: String = "localhost"
    val port: Int = 8080
  }
  def empty: LunarisConfigBox = LunarisConfigBox(ConfigFactory.empty())

  def default: LunarisConfigBox =
    empty
      .host.set(DefaultValues.host)
      .port.set(DefaultValues.port)
}
