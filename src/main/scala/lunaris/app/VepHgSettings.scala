package lunaris.app

import better.files.File
import lunaris.data.BlockGzippedWithIndex

final case class VepHgSettings(dataFileWithIndex: BlockGzippedWithIndex, fastaFile: File, dbNSFPFile: File,
                               exonsFile: File) {

}
