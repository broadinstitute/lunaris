package lunaris.app

import com.typesafe.config.{Config, ConfigFactory}
import lunaris.data.BlockGzippedWithIndex
import lunaris.genomics.Hg
import lunaris.io.{InputId, ResourceConfig}
import lunaris.utils.ConfigProps.{FileField, InputIdField, IntField, LunarisMiscModeField, LunarisModeField, StringField}
import lunaris.utils.{ConfigProps, SnagUtils}
import org.broadinstitute.yootilz.core.snag.Snag

import java.nio.channels.Channels
import java.nio.charset.StandardCharsets

case class LunarisConfigProps(config: Config) extends ConfigProps[LunarisConfigProps] {
  override def map(mapper: Config => Config): LunarisConfigProps = LunarisConfigProps(mapper(config))

  val mode: LunarisModeField[LunarisConfigProps] = LunarisModeField(this, "lunaris.mode")
  val configFile: InputIdField[LunarisConfigProps] = InputIdField(this, "lunaris.configFile")
  val requestFile: InputIdField[LunarisConfigProps] = InputIdField(this, "lunaris.batch.requestFile")
  val webInterface: StringField[LunarisConfigProps] = StringField(this, "lunaris.server.webInterface")
  val port: IntField[LunarisConfigProps] = IntField(this, "lunaris.server.port")
  val inputsFolder: FileField[LunarisConfigProps] = FileField(this, "lunaris.vep.inputsFolder")
  val resultsFolder: FileField[LunarisConfigProps] = FileField(this, "lunaris.vep.resultsFolder")
  val varIdField: StringField[LunarisConfigProps] = StringField(this, "lunaris.vep.field.varId")
  val posField: StringField[LunarisConfigProps] = StringField(this, "lunaris.vep.field.pos")
  val refField: StringField[LunarisConfigProps] = StringField(this, "lunaris.vep.field.ref")
  val altField: StringField[LunarisConfigProps] = StringField(this, "lunaris.vep.field.alt")
  val vepScriptFile: StringField[LunarisConfigProps] = StringField(this, "lunaris.vep.runVep.vepCmd")
  val vepWorkDir: FileField[LunarisConfigProps] = FileField(this, "lunaris.vep.runVep.workDir")
  val vepCacheDir: FileField[LunarisConfigProps] = FileField(this, "lunaris.vep.runVep.cacheDir")
  val vepPluginsDir: FileField[LunarisConfigProps] = FileField(this, "lunaris.vep.runVep.pluginsDir")
  val emailKeyId: StringField[LunarisConfigProps] = StringField(this, "lunaris.email.keyId")
  val emailKeyEncrypted: StringField[LunarisConfigProps] = StringField(this, "lunaris.email.keyEncrypted")
  val miscMode: LunarisMiscModeField[LunarisConfigProps] = LunarisMiscModeField(this, "lunaris.misc.mode")
  val exonsFile: FileField[LunarisConfigProps] = FileField(this, "lunaris.vep.runVep.exonsFile")
  val dbName: StringField[LunarisConfigProps] = StringField(this, "lunaris.vep.runVep.dbName")

  final case class HgProps(dataFile: InputIdField[LunarisConfigProps], indexFile: InputIdField[LunarisConfigProps],
                           fastaFile: FileField[LunarisConfigProps], dbNSFPFile: FileField[LunarisConfigProps],
                           exonsFile: FileField[LunarisConfigProps])

  object HgProps {
    def forHg(props: LunarisConfigProps, hg: Hg): HgProps = {
      val dataFile = InputIdField(props, s"lunaris.vep.${hg.name}.dataFile")
      val indexFile = InputIdField(props, s"lunaris.vep.${hg.name}.indexFile")
      val fastaFile = FileField(props, s"lunaris.vep.${hg.name}.fastaFile")
      val dbNSFPFile = FileField(props, s"lunaris.vep.${hg.name}.dbNSFPFile")
      val exonsFile = FileField(props, s"lunaris.vep.${hg.name}.exonsFile")
      HgProps(dataFile, indexFile, fastaFile, dbNSFPFile, exonsFile)
    }
  }

  val hg19Props: HgProps = HgProps.forHg(this, Hg.Hg19)
  val hg38Props: HgProps = HgProps.forHg(this, Hg.Hg38)

  def toServerSettings: Either[Snag, ServerSettings] = {
    for {
      hostVal <- webInterface.get
      portVal <- port.get
    } yield ServerSettings(hostVal, portVal)
  }

  def toHgSettings(hgProps: HgProps): Either[Snag, VepHgSettings] = {
    for {
      dataFile <- hgProps.dataFile.get
      indexFileOpt <- hgProps.indexFile.getOpt
      fastaFile <- hgProps.fastaFile.get
      dataFileWithIndex = BlockGzippedWithIndex(dataFile, indexFileOpt)
      dbNSFPFile <- hgProps.dbNSFPFile.get
      exonsFile <- hgProps.exonsFile.get
    } yield VepHgSettings(dataFileWithIndex, fastaFile, dbNSFPFile, exonsFile)
  }

  def toVepSettings: Either[Snag, VepSettings] = {
    for {
      inputsFolderVal <- inputsFolder.get
      resultsFolderVal <- resultsFolder.get
      varIdFieldVal <- varIdField.get
      posFieldVal <- posField.get
      refFieldVal <- refField.get
      altFieldVal <- altField.get
      vepDataFields = VepDataFieldsSettings(varIdFieldVal, posFieldVal, refFieldVal, altFieldVal)
      vepScriptFile <- vepScriptFile.get
      workDir <- vepWorkDir.get
      cacheDir <- vepCacheDir.get
      pluginsDir <- vepPluginsDir.get
      exonsFile <- exonsFile.get
      vepRunSettings = VepRunSettings(vepScriptFile, workDir, cacheDir, pluginsDir)
      hg19Settings <- toHgSettings(hg19Props)
      hg38Settings <- toHgSettings(hg38Props)
    } yield VepSettings(inputsFolderVal, resultsFolderVal, vepDataFields, vepRunSettings, hg19Settings, hg38Settings)
  }

  def toEmailSettings: Either[Snag, EmailSettings] = {
    for {
      emailKeyId <- emailKeyId.get
      emailKeyEncrypted <- emailKeyEncrypted.get
    } yield EmailSettings(emailKeyId, emailKeyEncrypted)
  }

  def toVepServerSettings: Either[Snag, VepServerSettings] = {
    for {
      serverSettings <- toServerSettings
      vepSettings <- toVepSettings
      emailSettings <- toEmailSettings
      dbName <- dbName.get
    } yield VepServerSettings(serverSettings, vepSettings, emailSettings, dbName)
  }
}

object LunarisConfigProps {

  object FallbackValues {
    val webInterface: String = "0.0.0.0"
    val port: Int = 8080
    val dbName: String = "egg"
  }

  def empty: LunarisConfigProps = LunarisConfigProps(ConfigFactory.empty())

  def cliProps(args: Seq[String]): Either[Snag, LunarisConfigProps] =
    SnagUtils.tryOrSnag(new LunarisCliConf(args).toConfigProps)

  def inputIdProps(in: InputId, resourceConfig: ResourceConfig = ResourceConfig.empty):
  Either[Snag, LunarisConfigProps] = {
    for {
      readChannelDisposable <- SnagUtils.tryOrSnag(in.newReadChannelDisposable(resourceConfig))
      configProps <- readChannelDisposable.useUp { readChannel =>
        SnagUtils.tryOrSnag {
          val reader = Channels.newReader(readChannel, StandardCharsets.UTF_8)
          val config = ConfigFactory.parseReader(reader)
          LunarisConfigProps(config)
        }
      }
    } yield configProps
  }

  def defaultSourcesProps(): Either[Snag, LunarisConfigProps] =
    SnagUtils.tryOrSnag(LunarisConfigProps(ConfigFactory.load))

  def fallbackProps: LunarisConfigProps = {
    empty
      .webInterface.set(FallbackValues.webInterface)
      .port.set(FallbackValues.port)
      .dbName.set(FallbackValues.dbName)
  }

  def allProps(args: Seq[String], resourceConfig: ResourceConfig = ResourceConfig.empty):
  Either[Snag, LunarisConfigProps] = {
    for {
      cliProps <- cliProps(args)
      defaultSourcesProps <- defaultSourcesProps()
      lowPrecedenceProps = defaultSourcesProps.withFallback(fallbackProps)
      preliminaryAllProps = cliProps.withFallback(lowPrecedenceProps)
      configFileOpt <- preliminaryAllProps.configFile.getOpt
      allProps <- configFileOpt match {
        case None => Right(cliProps.withFallback(lowPrecedenceProps))
        case Some(configFile) =>
          println(configFile)
          for {
            inputIdProps <- inputIdProps(configFile, resourceConfig)
          } yield cliProps.withFallback(inputIdProps).withFallback(lowPrecedenceProps)
      }
    } yield allProps
  }

}
