package lunaris.app

import better.files.File

case class VepRunSettings(vepCmd: String, workDir: File, fastaFile: File, cacheDir: File, pluginsDir: File,
                          dbNSFPFile: File, exonsFile: File)
