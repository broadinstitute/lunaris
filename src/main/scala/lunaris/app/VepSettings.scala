package lunaris.app

import better.files.File
import lunaris.genomics.Hg

case class VepSettings(inputsFolder: File, resultsFolder: File, vepDataFieldsSettings: VepDataFieldsSettings,
                       runSettings: VepRunSettings, hg19Settings: VepHgSettings, hg38Settings: VepHgSettings) {
  def hgSettings(hg: Hg): VepHgSettings = {
    hg match {
      case Hg.Hg19 => hg19Settings
      case Hg.Hg38 => hg38Settings
    }
  }
}
