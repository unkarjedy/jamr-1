package scripts.DO_NOT_LOOK_HERE

import java.io.{File, PrintStream}

import edu.cmu.lti.nlp.amr.utils.CorpusUtils
import org.apache.commons.lang3.StringUtils
import scripts.utils.FileExt._

import scala.io.Source

object SntIdSetter {

  def main(args: Array[String]): Unit = {
    setSentencesId()
    //updateAmrIDs()
  }

  private def updateAmrIDs(): Unit = {
    val baseFolder = new File(".").resolve("resources_terms/FinalOutputs/gold_amr/")
    val fileName = "sentences2.txt.amr_gold_WIP"

    val in = Source.fromFile(baseFolder.resolve(fileName))
    val out = new PrintStream(baseFolder.resolve(fileName + ".with_snt-id"))

    val StartSntId: Int = 114
    var sntId = StartSntId

    val blocks: Iterator[String] = CorpusUtils.splitOnNewline(in).dropWhile(StringUtils.isBlank)

    blocks.foreach { block =>
      val blockUpdated = block.lines.find(_.startsWith("# ::sntId")) match {
        case Some(line) =>
          val res = block.replace(line, s"# ::sntId $sntId")
          sntId += 1
          res
        case None =>
          block
      }
      out.println(blockUpdated)
      out.println
    }
  }

  private def setSentencesId(): Unit = {
    val baseFolder = new File(".").resolve("resources_terms/FinalOutputs/sentences/")
    val fileName = "sentences3.txt"

    val in = Source.fromFile(baseFolder.resolve(fileName))
    val out = new PrintStream(baseFolder.resolve(fileName + ".with_snt-id"))

    val StartSntId: Int = 218
    var sntId = StartSntId

    val blocks: Iterator[String] = CorpusUtils.splitOnNewline(in).dropWhile(StringUtils.isBlank)

    blocks.foreach { block =>
      val blockUpdated =
        if (block.startsWith("# format")) {
          block
        } else {
          val str = s"# ::sntId $sntId\n" + block
          sntId += 1 // no time to play a FunctionalStyle hero...
          str
        }
      out.println(blockUpdated)
      out.println
    }
  }
}
