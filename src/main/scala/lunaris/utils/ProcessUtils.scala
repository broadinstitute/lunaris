package lunaris.utils

import better.files.File

import scala.sys.process._

object ProcessUtils {
  def ls(file: File): String = s"ls -l $file".!!.trim
}
