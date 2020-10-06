package lunaris.app

import better.files.File
import lunaris.data.BlockGzippedWithIndex

case class VepSettings(inputsFolder: File, resultsFolder: File, dataFileWithIndex: BlockGzippedWithIndex,
                       vepDataFieldsSettings: VepDataFieldsSettings, runSettings: VepRunSettings)
