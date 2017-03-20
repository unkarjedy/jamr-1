package scripts.utils.logger

import java.util.logging.{Formatter, LogRecord}

class UnmodifyingFormatter extends Formatter {
  override def format(record: LogRecord): String = {
    val sb = new StringBuilder()
    sb.append(record.getMessage).append('\n')
    sb.toString()
  }
}
