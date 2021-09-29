package lunaris.app

import better.files.File

case class VepRunSettings(vepCmd: String, workDir: File, cacheDir: File, pluginsDir: File) {
  def runDir: File = workDir / "run"
}
