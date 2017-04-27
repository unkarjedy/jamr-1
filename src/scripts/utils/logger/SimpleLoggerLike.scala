package scripts.utils.logger

import java.util.logging.Logger


trait SimpleLoggerLike {
  protected val logger: Logger = Logger.getLogger(this.getClass.getSimpleName)
  initLogger()

  def initLogger(): Unit = {
    logger.getParent.getHandlers.foreach(_.setFormatter(new SimpleFormatter()))
  }
}
