package lunaris.utils

import java.util.Date

object DateUtils {

  def timeToString(time: Long): String = new Date(time).toString

  def timeDiffToString(timeDiff: Long): String = {
    if(timeDiff < 1000) {
      s"${timeDiff}ms"
    } else {
      val msRemain = timeDiff % 1000L
      val seconds = timeDiff / 1000L
      val msRemainString = msRemain.toString
      val msRemainPadded = "0" * (3 - msRemainString.length) + msRemainString
      if(seconds < 100L) {
        s"$seconds.${msRemainPadded}s"
      } else {
        val minutes = seconds / 60L
        val secondsRemain = seconds % 60L
        if(minutes < 60L) {
          s"${minutes}m${secondsRemain}s"
        } else {
          val hours = minutes / 60L
          val minutesRemain = minutes % 60L
          if(hours < 24L) {
            s"${hours}h${minutesRemain}m${secondsRemain}s"
          } else {
            val days = hours / 24L
            val hoursRemain = hours % 24L
            s"${days}d${hoursRemain}h${minutesRemain}m"
          }
        }
      }
    }
  }
}
