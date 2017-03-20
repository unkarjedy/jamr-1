package scripts.utils.logger

import java.util.logging.{Formatter, LogRecord}

class SimpleFormatter extends Formatter {
  override def format(record: LogRecord): String = {
    val sb = new StringBuilder()
    sb.append(record.getLevel).append(':')
    sb.append(record.getMessage).append('\n')
    sb.toString()
  }
}
