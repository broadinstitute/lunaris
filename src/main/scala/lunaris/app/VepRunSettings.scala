package lunaris.app

import better.files.File

case class VepRunSettings(vepCmd: String, workDir: File, fastaFile: File, pluginsDir: File, dbNSFPFile: File)
