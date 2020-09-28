package lunaris.app

import java.nio.channels.Channels
import java.nio.charset.StandardCharsets

import com.typesafe.config.{Config, ConfigFactory}
import lunaris.data.BlockGzippedWithIndex
import lunaris.io.{InputId, ResourceConfig}
import lunaris.utils.{ConfigProps, SnagUtils}
import lunaris.utils.ConfigProps.{FileField, InputIdField, IntField, LunarisModeField, StringField}
import org.broadinstitute.yootilz.core.snag.Snag

case class LunarisConfigProps(config: Config) extends ConfigProps[LunarisConfigProps] {
  override def map(mapper: Config => Config): LunarisConfigProps = LunarisConfigProps(mapper(config))

  val mode: LunarisModeField[LunarisConfigProps] = LunarisModeField(this, "lunaris.mode")
  val configFile: InputIdField[LunarisConfigProps] = InputIdField(this, "lunaris.configFile")
  val requestFile: InputIdField[LunarisConfigProps] = InputIdField(this, "lunaris.batch.requestFile")
  val webInterface: StringField[LunarisConfigProps] = StringField(this, "lunaris.server.webInterface")
  val port: IntField[LunarisConfigProps] = IntField(this, "lunaris.server.port")
  val inputsFolder: FileField[LunarisConfigProps] = FileField(this, "lunaris.vep.inputsFolder")
  val resultsFolder: FileField[LunarisConfigProps] = FileField(this, "lunaris.vep.resultsFolder")
  val dataFile: InputIdField[LunarisConfigProps] = InputIdField(this, "lunaris.vep.dataFile")
  val indexFile: InputIdField[LunarisConfigProps] = InputIdField(this, "lunaris.vep.indexFile")
  val varId: StringField[LunarisConfigProps] = StringField(this, "lunaris.vep.varId")

  def toServerSettings: Either[Snag, ServerSettings] = {
    for {
      hostVal <- webInterface.get
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

object LunarisConfigProps {
  object FallbackValues {
    val webInterface: String = "0.0.0.0"
    val port: Int = 8080
  }
  def empty: LunarisConfigProps = LunarisConfigProps(ConfigFactory.empty())

  def cliProps(args: Array[String]): Either[Snag, LunarisConfigProps] =
    SnagUtils.tryOrSnag(new LunarisCliConf(args).toConfigProps)

  def inputIdProps(in: InputId, resourceConfig: ResourceConfig): Either[Snag, LunarisConfigProps] = {
    in.newReadChannelDisposable(resourceConfig).useUp { readChannel =>
      SnagUtils.tryOrSnag {
        val reader = Channels.newReader(readChannel, StandardCharsets.UTF_8)
        val config = ConfigFactory.parseReader(reader)
        LunarisConfigProps(config)
      }
    }
  }

  def defaultSourcesProps(): Either[Snag, LunarisConfigProps] =
    SnagUtils.tryOrSnag(LunarisConfigProps(ConfigFactory.load))

  def fallbackProps: LunarisConfigProps = {
    empty
      .webInterface.set(FallbackValues.webInterface)
      .port.set(FallbackValues.port)
  }

  def allProps(args: Array[String]): Either[Snag, LunarisConfigProps] = {
    for {
      cliProps <- cliProps(args)
      defaultSourcesProps <- defaultSourcesProps()
      preliminaryAllProps = cliProps.withFallback(defaultSourcesProps).withFallback(fallbackProps)
    } yield cliProps.withFallback(defaultSourcesProps).withFallback(fallbackProps)
    //  TODO
  }

}
