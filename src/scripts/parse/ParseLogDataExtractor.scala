package scripts.parse

import java.io.{File, PrintStream}

import edu.cmu.lti.nlp.amr.utils.CorpusUtils
import scripts.parse.ParseLogDataExtractor.Prefixes
import scripts.parse.ParseLogDataExtractor.Prefixes._

import scala.io.Source
import scala.util.{Failure, Success, Try}

class ParseLogDataExtractor(folderWithLogs: String, parsedFilename: String) {
  private val folderWithLogsPath = new File(folderWithLogs).toPath
  private val outLogFile = folderWithLogsPath.resolve(s"$parsedFilename.out").toFile
  private val errLogFile = folderWithLogsPath.resolve(s"$parsedFilename.err").toFile

  def extractAmrOnly(): Unit = Try {
    def buildPringStream(suffix: String) = {
      new PrintStream(folderWithLogsPath.resolve(s"$parsedFilename.$suffix").toFile)
    }

    lazy val printStreamAll = buildPringStream("amr_deps_all")
    lazy val printStreamDepsGold = buildPringStream("amr_deps_gold")
    lazy val printStreamDeps0 = buildPringStream("amr_deps_0")
    lazy val printStreamDeps1 = buildPringStream("amr_deps_1")
    lazy val printStreamDeps2 = buildPringStream("amr_deps_2")
    lazy val printStreamDeps3 = buildPringStream("amr_deps_3")
    lazy val printStreamDeps4 = buildPringStream("amr_deps_4")

    CorpusUtils.splitOnNewline(Source.fromFile(outLogFile).getLines()).foreach { block =>
      val lines = block.lines.toArray
      val treeId = extractTreeId(lines)

      printBlockLines(printStreamAll, lines)
      treeId match {
        case Some("gold") => printBlockLines(printStreamDepsGold, lines)
        case Some("0") => printBlockLines(printStreamDeps0, lines)
        case Some("1") => printBlockLines(printStreamDeps1, lines)
        case Some("2") => printBlockLines(printStreamDeps2, lines)
        case Some("3") => printBlockLines(printStreamDeps3, lines)
        case Some("4") => printBlockLines(printStreamDeps4, lines)
        case _ =>
      }
    }
  } match {
    case Success(_) =>
    case Failure(ex) =>
      ex.printStackTrace(System.err)
  }

  private def extractTreeId(lines: Array[String]): Option[String] = {
    lines.find(_.startsWith(TreeIdPrefix))
      .map(_.substring(TreeIdPrefix.length).trim.toLowerCase())
  }

  private def printBlockLines(printStream: PrintStream, amrGroup: Array[String]) = {
    val (metaLines, amrLines) = amrGroup.partition(_.startsWith("#"))
    val metaLinesExtended =
      metaLines.filter(s => Prefixes.all.exists(s.startsWith(_))) :+ "# ::src jamr"
    (metaLinesExtended ++ amrLines).foreach(printStream.println)
    printStream.println()
  }
}

object ParseLogDataExtractor {
  object Prefixes {
    val SntPrefix = "# ::snt "
    val TermPrefix = "# ::term "
    val SntIdPrefix = "# ::sntId "
    val TreeIdPrefix = "# ::treeId "

    val all = Seq(SntPrefix, TermPrefix, SntIdPrefix, TreeIdPrefix)
  }
}
