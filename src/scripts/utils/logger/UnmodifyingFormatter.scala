package scripts.utils.logger

import java.util.logging.{Formatter, LogRecord}

class UnmodifyingFormatter extends Formatter {
  override def format(record: LogRecord): String = {
    record.getMessage + "\n"
  }
}
