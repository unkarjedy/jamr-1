package scripts.parse

import java.io.{File, PrintStream}

import org.apache.commons.lang3.StringUtils

import scala.io.Source
import scala.util.{Failure, Success, Try}

class ParseLogDataExtractor(folderWithLogs: String, parsedFilename: String) {
  private val folderWithLogsPath = new File(folderWithLogs).toPath
  private val outLogFile = folderWithLogsPath.resolve(s"$parsedFilename.out").toFile
  private val errLogFile = folderWithLogsPath.resolve(s"$parsedFilename.err").toFile

  val SNT_PREFIX = "# ::snt "

  def extractAmrOnly(): Unit = Try {
    val printStream = new PrintStream(folderWithLogsPath.resolve(s"$parsedFilename.amr_only").toFile)
    Source.fromFile(outLogFile).getLines().foreach {
      case s if (s.startsWith(SNT_PREFIX)) =>
        printStream.println(s.substring(SNT_PREFIX.length))
      case s if !s.startsWith("#") =>
        printStream.println(s)
      case _ =>
    }
  } match {
    case Success(_) =>
    case Failure(ex) =>
      ex.printStackTrace(System.err)
  }


}
