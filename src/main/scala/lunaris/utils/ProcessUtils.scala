package lunaris.utils

import better.files.File

import scala.sys.process._
import scala.util.Try

object ProcessUtils {
  def ls(file: File): Try[String] = {
    Try {
      s"ls -l $file".!!.trim
    }
  }
}
