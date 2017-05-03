package scripts.utils

import java.util.concurrent.TimeUnit
import java.util.logging.Logger

object TimeUtils {
  def runWithTimer[T](logger: Logger)(callback: => T): T = {
    val now = System.nanoTime
    val result = callback
    val timeSpentNs = System.nanoTime() - now
    logger.info(s"Stage time: ${timeNsToString(timeSpentNs)}")
    result
  }

  private def timeNsToString(timeNs: Long) = {
    val timeMin = TimeUnit.NANOSECONDS.toMinutes(timeNs)
    val timeSec = TimeUnit.NANOSECONDS.toSeconds(timeNs)
    var secRamain = timeSec - TimeUnit.MINUTES.toSeconds(timeMin)

    s"$timeMin min, $secRamain sec"
  }
}
