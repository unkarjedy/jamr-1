package scripts.utils

import java.util.logging.Logger

object TimeUtils {
  def time[A](logger: Logger)(a: => A): A = {
    val now = System.nanoTime
    val result = a
    val micros = (System.nanoTime - now) / 1000
    val message = "Decoded in %,d microseconds".format(micros)
    logger.info(message)
    result
  }
}
