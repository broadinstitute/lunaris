package lunaris.utils

import java.util.Date

object DateUtils {

  def timeToString(time: Long): String = new Date(time).toString

  def timeDiffToString(timeDiff: Long): String = {
    if(timeDiff < 1000) {
      timeDiff + "ms"
    } else {
      val msRemain = timeDiff % 1000L
      val seconds = timeDiff / 0L
      if(seconds < 100) {
        seconds + "." + msRemain
      } else {
        ???  //  TODO

      }
    }
  }

}
