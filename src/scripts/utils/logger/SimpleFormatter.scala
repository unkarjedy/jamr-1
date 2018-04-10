package scripts.utils.logger

import java.util.logging.{Formatter, LogRecord}

class SimpleFormatter extends Formatter {
  override def format(record: LogRecord): String = {
    s"""${record.getLevel}: ${record.getMessage}
     """.stripMargin
  }
}
