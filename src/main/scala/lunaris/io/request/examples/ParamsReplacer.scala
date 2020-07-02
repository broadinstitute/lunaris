package lunaris.io.request.examples

import java.io.InputStream

import lunaris.utils.ReplacerInputStream
import lunaris.utils.ReplacerInputStream.ReplacerMap

object ParamsReplacer {

  private def getEntry(params: Map[String, String], in: String, replace: String, key: String):
  Option[ReplacerMap.Entry] = {
    params.get(key).map(ReplacerMap.Entry(in, replace, _))
  }

  def getInputStreamFilter(params: Map[String, String]): InputStream => InputStream = {
    val entries = Seq(
      getEntry(params, "\"1\"", "1", "chr"),
      getEntry(params, "\"begin\" : 100000", "100000", "begin"),
      getEntry(params, "\"end\" : 200000", "200000", "end"),
      getEntry(params, "\"stringValue\" : \"T2D\"", "T2D", "trait")
    ).flatten
    if (entries.isEmpty) {
      is: InputStream => is
    } else {
      is: InputStream => new ReplacerInputStream(is, ReplacerMap.fromEntries(entries))
    }
  }

}
