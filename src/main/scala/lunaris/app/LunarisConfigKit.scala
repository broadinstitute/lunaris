package lunaris.app

import com.typesafe.config.{Config, ConfigFactory}
import lunaris.data.BlockGzippedWithIndex
import lunaris.utils.ConfigKit
import lunaris.utils.ConfigKit.{FileField, InputIdField, IntField, LunarisModeField, StringField}
import org.broadinstitute.yootilz.core.snag.Snag

case class LunarisConfigKit(config: Config) extends ConfigKit[LunarisConfigKit] {
  override def map(mapper: Config => Config): LunarisConfigKit = LunarisConfigKit(mapper(config))

  val mode: LunarisModeField[LunarisConfigKit] = LunarisModeField(this, "lunaris.mode")
  val host: StringField[LunarisConfigKit] = StringField(this, "lunaris.server.host")
  val port: IntField[LunarisConfigKit] = IntField(this, "lunaris.server.port")
  val inputsFolder: FileField[LunarisConfigKit] = FileField(this, "lunaris.vep.inputsFolder")
  val resultsFolder: FileField[LunarisConfigKit] = FileField(this, "lunaris.vep.resultsFolder")
  val dataFile: InputIdField[LunarisConfigKit] = InputIdField(this, "lunaris.vep.dataFile")
  val indexFile: InputIdField[LunarisConfigKit] = InputIdField(this, "lunaris.vep.indexFile")
  val varId: StringField[LunarisConfigKit] = StringField(this, "lunaris.vep.varId")

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
      dataFileVal <- dataFile.get
      indexFileOpt <- indexFile.getOpt
      dataFileWithIndex = BlockGzippedWithIndex(dataFileVal, indexFileOpt)
      varIdVal <- varId.get
    } yield VepSettings(inputsFolderVal, resultsFolderVal, dataFileWithIndex, varIdVal)
  }

  def toVepServerSettings: Either[Snag, VepServerSettings] = {
    for {
      serverSettings <- toServerSettings
      vepSettings <- toVepSettings
    } yield VepServerSettings(serverSettings, vepSettings)
  }
}

object LunarisConfigKit {
  object DefaultValues {
    val host: String = "localhost"
    val port: Int = 8080
  }
  def empty: LunarisConfigKit = LunarisConfigKit(ConfigFactory.empty())

  def default: LunarisConfigKit =
    empty
      .host.set(DefaultValues.host)
      .port.set(DefaultValues.port)
}
