package scripts.utils.logger

import java.util.logging.Logger


trait SimpleLoggerLike {
  protected val logger: Logger = Logger.getLogger(this.getClass.getSimpleName)
  initLogger()

  def initLogger(): Unit = {
    logger.getHandlers.foreach(_.setFormatter(new SimpleFormatter()))
    logger.getParent.getHandlers.foreach(_.setFormatter(new SimpleFormatter()))
  }
}
