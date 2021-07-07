package lunaris.utils

import org.broadinstitute.yootilz.core.snag.Snag

import java.util.Date

object DebugUtils {
  def printSnag(preface: String, snag: Snag): Unit = {
    printlnAny(preface)
    printlnAny(snag.message)
    printlnAny(snag.report)
  }

  def printlnAny(any: Any): Unit = println(any)

  def printlnDebug(string: String): Unit = println(s"======= ${new Date()}: $string.")
}
