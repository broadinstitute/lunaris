package musha

import better.files.File

case class MushaConfig(dbFilePath: File, user: String, password: String)
