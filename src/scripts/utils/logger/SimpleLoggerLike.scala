package scripts.utils.logger

import java.util.logging.Logger


trait SimpleLoggerLike {
  protected val logger = Logger.getLogger(this.getClass.getSimpleName)
  logger.getParent.getHandlers.foreach(_.setFormatter(new SimpleFormatter()))
}
