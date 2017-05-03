package scripts.utils

import scripts.utils.TimeUtils.runWithTimer
import scripts.utils.logger.SimpleLoggerLike

trait StageRunnerLike extends SimpleLoggerLike{
  protected def runStage[A](logString: String, skip: Boolean)(stageFunc: => A): Unit = {
    if(!skip){
      logger.info(s"$logString")
      runWithTimer(logger)(stageFunc)
    } else {
      logger.info(s"$logString [Skipped]")
    }
  }
}
