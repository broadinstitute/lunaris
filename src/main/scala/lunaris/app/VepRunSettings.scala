package lunaris.app

import better.files.File

case class VepRunSettings(vepScriptFile: File, workDir: File, fastaFile: File, pluginsDir: File, dbNSFPFile: File)
