package lunaris.vep

import lunaris.app.VepRunSettings
import org.broadinstitute.yootilz.core.snag.Snag

object VepRunSettingsBox {

  private var vepRunSettingsOpt: Option[VepRunSettings] = None

  def setVepRunSettings(vepRunSettings: VepRunSettings): Unit = {
    vepRunSettingsOpt = Some(vepRunSettings)
  }

  def getVepRunSettings: Either[Snag, VepRunSettings] = {
    vepRunSettingsOpt match {
      case Some(vepRunSettings) => Right(vepRunSettings)
      case None => Left(Snag(s"No VEP run settings present."))
    }
  }

}
