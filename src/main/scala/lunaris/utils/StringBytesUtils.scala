package lunaris.utils

import java.io.{InputStream, StringReader}

import better.files.ReaderInputStream

object StringBytesUtils {
  def stringToInputStream(string: String): InputStream = new ReaderInputStream(new StringReader(string))
  def inputStreamToString(is: InputStream): String = new String(is.readAllBytes())
}
