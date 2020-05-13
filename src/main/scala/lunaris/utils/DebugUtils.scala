package lunaris.utils

import org.broadinstitute.yootilz.core.snag.Snag

object DebugUtils {
  def printSnag(preface: String, snag: Snag): Unit = {
    println(preface)
    println(snag.message)
    println(snag.report)
  }

  def println(any: Any): Unit = Predef.println(any)
}
