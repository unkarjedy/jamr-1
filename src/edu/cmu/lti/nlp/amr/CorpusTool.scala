package edu.cmu.lti.nlp.amr

import java.io.{InputStream, PrintStream}

import edu.cmu.lti.nlp.amr.Corpus._
import edu.cmu.lti.nlp.amr.CorpusTool._

import scala.collection.mutable.Map

class CorpusTool(in: InputStream,
                 out: PrintStream,
                 tokenizedFileName: String) extends Runnable {

  override def run(): Unit = {
    val tokenizedFileLines = Source.fromFile(tokenizedFileName).getLines.toArray

    var amrBlockId = 0
    val blocks = splitOnNewline(Source.fromInputStream(in).getLines)
    for (block <- blocks) {
      if (doesContainSomeAmr(block)) {
        val split = block.split(AMR_BEGINNING)
        val extras = split.head
        val amr = split.tail.mkString("\n(") // needs to contain come AMR
        out.println(extras)
        out.println(s"# ::tok ${tokenizedFileLines(amrBlockId)}")
        out.println(s"($amr\n")
        amrBlockId += 1
      } else {
        out.println(block + "\n")
      }
    }
  }
}

object CorpusTool {
  private val usage =
    """Usage: scala -classpath . \
      |edu.cmu.lti.nlp.amr.CorpusTool \
      |--tokenized tokenized_sentences \
      |< amr_corpus \
      |> new_amr_corpus
      |""".stripMargin

  type OptionMap = Map[Symbol, Any]

  def parseOptions(map: OptionMap, args: List[String]): OptionMap = {
    args match {
      case Nil => map
      case "--tokenized" :: value :: tail => parseOptions(map ++ Map('tokenized -> value), tail)
      case "-v" :: value :: tail => parseOptions(map ++ Map('verbosity -> value.toInt), tail)
      case option :: tail =>
        System.out.println("Error: Unknown option " + option)
        sys.exit(1)
    }
  }

  /* Actually this script only reads and writes info, no super logic is presented */
  def main(args: Array[String]) {
    validateArgs(args)

    val options = parseOptions(Map(), args.toList)
    if (options.contains('verbosity)) {
      verbosityGlobal = options('verbosity).asInstanceOf[Int]
    }
    if (!options.contains('tokenized)) {
      System.err.println("Error: No tokenized file specified")
      sys.exit(1)
    }

    val tool = new CorpusTool(System.in,
                              System.out,
                              options('tokenized).asInstanceOf[String])
    tool.run()
  }

  private def validateArgs(args: Array[String]) = {
    if (args.length == 0) {
      System.out.println(usage)
      sys.exit(1)
    }
  }
  private val AMR_BEGINNING = "\n[(]"

  def doesContainSomeAmr(block: String): Boolean = {
    block.split("\n").exists(_.startsWith("("))
  }

}

