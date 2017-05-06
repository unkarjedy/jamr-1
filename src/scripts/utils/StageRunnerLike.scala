package scripts.utils

import scripts.utils.TimeUtils.runWithTimeLogging
import scripts.utils.logger.SimpleLoggerLike

trait StageRunnerLike extends SimpleLoggerLike {
  protected def runStage[A](logString: String, skip: Boolean)(stageFunc: => A): Unit = {
    if (!skip) {
      logger.info(s"$logString")
      runWithTimeLogging(logger)(stageFunc)
    } else {
      logger.info(s"$logString [Skipped]")
    }
  }

  protected def tryRunStage[A](logString: String, skip: Boolean)(stageFunc: => A): Unit = {
    try {
      runStage(logString, skip)(stageFunc)
    } catch {
      case e: Exception => logger.info(s"ERROR: ($logString) finished with exception]")
    }
  }
}
