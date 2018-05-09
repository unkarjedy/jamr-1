package scripts.parse

import java.io.{File, PrintStream}

import edu.cmu.lti.nlp.amr.utils.CorpusUtils

import scala.io.Source
import scala.util.{Failure, Success, Try}

class ParseLogDataExtractor(folderWithLogs: String, parsedFilename: String) {
  private val folderWithLogsPath = new File(folderWithLogs).toPath
  private val outLogFile = folderWithLogsPath.resolve(s"$parsedFilename.out").toFile
  private val errLogFile = folderWithLogsPath.resolve(s"$parsedFilename.err").toFile

  def extractAmrOnly(): Unit = Try {
    val printStream = new PrintStream(folderWithLogsPath.resolve(s"$parsedFilename.amr_only").toFile)
    val printStreamOnly1 = new PrintStream(folderWithLogsPath.resolve(s"$parsedFilename.amr_only_1").toFile)
    val printStreamOnlyGold = new PrintStream(folderWithLogsPath.resolve(s"$parsedFilename.amr_only_gold").toFile)

    CorpusUtils.splitOnNewline(Source.fromFile(outLogFile).getLines()).foreach { block =>
      val lines = block.lines.toArray
      printBlockLines(printStream, lines)

      // printing only AMRs for first dependency tree
      lines.find(_.startsWith(Prefixes.TreeIdPrefix)) match {
         case Some(x) if x.substring(Prefixes.TreeIdPrefix.length).trim == "0" =>
          printBlockLines(printStreamOnly1, lines)
        case _ =>
      }

      // printing only GOLD AMRs
      lines.find(_.startsWith(Prefixes.TreeIdPrefix)) match {
         case Some(x) if x.substring(Prefixes.TreeIdPrefix.length).trim.toLowerCase == "gold" =>
          printBlockLines(printStreamOnlyGold, lines)
        case _ =>
      }
    }
  } match {
    case Success(_) =>
    case Failure(ex) =>
      ex.printStackTrace(System.err)
  }

  private def printBlockLines(printStream: PrintStream, amrGroup: Array[String]) = {
    val (metaLines, amrLines) = amrGroup.partition(_.startsWith("#"))
    val metaLinesExtended =
      metaLines.filter(s => Prefixes.all.exists(s.startsWith(_))) :+ "# ::src jamr"
    (metaLinesExtended ++ amrLines).foreach(printStream.println)
    printStream.println()
  }

  object Prefixes {
    val SntPrefix = "# ::snt "
    val TermPrefix = "# ::term "
    val SntIdPrefix = "# ::sntId "
    val TreeIdPrefix = "# ::treeId "

    val all = Seq(SntPrefix, TermPrefix, SntIdPrefix, TreeIdPrefix)
  }
}
