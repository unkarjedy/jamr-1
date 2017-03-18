package edu.cmu.lti.nlp.amr.align

import java.io.{InputStream, PrintStream}
import java.text.SimpleDateFormat
import java.util.Date

import edu.cmu.lti.nlp.amr.align.Aligner.{OptionMap, dateFormat}
import edu.cmu.lti.nlp.amr.graph.Graph
import edu.cmu.lti.nlp.amr.utils.JAMRLogger
import edu.cmu.lti.nlp.amr.{AMRTrainingData, Corpus, CorpusTool, Source}

import scala.collection.mutable
import scala.collection.mutable.Map

class Aligner(in: InputStream,
              out: PrintStream,
              err: PrintStream,
              useAligner3: Boolean,
              verbosity: Int,
              options: OptionMap) extends Runnable {

  private val logger = JAMRLogger(err, verbosity)
  private val log = logger.log _

  private val spansAligner = SpansAligner(logger)
  private val spansAligner3 = SpansAlignerWrapper3(logger)
  private val wordsAligner = WordsAligner(logger)

  override def run(): Unit = {
    val blocks = Corpus.splitOnNewline(Source.fromInputStream(in).getLines)

    val sdf = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS")
    for (block <- blocks) {
      if (CorpusTool.doesContainSomeAmr(block)) {
        log(2, "**** Processsing Block *****")
        log(2, block)
        log(2, "****************************")

        val blockLines = block.split("\n")
        val extraStr = blockLines.filter(_.matches("^# ::.*")).mkString("\n")
        val amrStr = blockLines.filterNot(_.matches("^#.*")).mkString("\n")

        val amr = Graph.parse(amrStr)
        val extras = AMRTrainingData.getUlfString(extraStr)

        val tokenized = extras("::tok").split(" ")
        val wordAlignments = wordsAligner.alignWords(tokenized, amr)
        val spanAlignments =
          if (useAligner3) {
            spansAligner3.align(tokenized, amr)
          } else {
            spansAligner.alignSpans(tokenized, amr, wordAlignments)
          }
        spansAligner.logUnalignedConcepts(amr.root)

        val spans = amr.spans
        for ((span, i) <- spans.zipWithIndex) {
          log(1, s"Span ${(i + 1).toString}:  ${span.words} => ${span.amrNode}")
          log(3, "* " + span.format)
        }

        out.println(extraStr)
        if (useAligner3) {
          println("# ::alignments " + spans.map(_.format()).mkString(" ") + " ::annotator Aligner v.03 ::date " + sdf.format(new Date))
        } else {
          println("# ::alignments " + spans.map(_.format()).mkString(" ") + " ::annotator Aligner v.01 ::date " + sdf.format(new Date))
        }
        if (options.contains('logUnalignedConcepts)) {
          amr.logUnalignedNodes()
        }
        if (options.contains('printNodesAndEdges)) {
          println(amr.printNodes.map(x => "# ::node\t" + x).mkString("\n"))
          println(amr.printRoot)
          if (amr.root.relations.size > 0) {
            println(amr.printEdges.map(x => "# ::edge\t" + x).mkString("\n"))
          }
        }
        out.println(amrStr + "\n")
      } else {
        out.println(block + "\n")
      }
    }
  }
}

/** **************************** Driver Program *****************************/
object Aligner {
  private val usage =
    """Usage: scala -classpath .
      |edu.cmu.lti.nlp.amr.align.Aligner
      |< amr_file
      |> alignments""".stripMargin

  private val dateFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS")
  type OptionMap = Map[Symbol, Any]

  def parseOptions(map: OptionMap, list: List[String]): OptionMap = {
    def isSwitch(s: String) = s(0) == '-'
    list match {
      case Nil => map
      case "-h" :: value :: tail => parseOptions(map ++ Map('help -> value.toInt), tail)
      case "-1" :: tail => parseOptions(map ++ Map('aligner1 -> true), tail)
      case "--print-nodes-and-edges" :: tail => parseOptions(map ++ Map('printNodesAndEdges -> true), tail)
      case "--log-unaligned" :: tail => parseOptions(map ++ Map('logUnalignedConcepts -> true), tail)
      case "-v" :: value :: tail => parseOptions(map ++ Map('verbosity -> value.toInt), tail)
      //case "--train" :: tail =>
      //          parseOptions(map ++ Map('train -> true), tail)
      //case "-a" :: value :: tail =>
      //          parseOptions(map ++ Map('amrfile -> value), tail)
      //case "--only" :: tail =>
      //          parseOptions(map ++ Map('only -> true), tail)
      //case string :: opt2 :: tail if isSwitch(opt2) =>
      //          parseOptions(map ++ Map('infile -> string), list.tail)
      //case string :: Nil =>  parseOptions(map ++ Map('infile -> string), list.tail)
      case option :: tail =>
        println("Error: Unknown option " + option)
        sys.exit(1)
    }
  }

  def main(args: Array[String]) {
    val options = parseOptions(Map(), args.toList)
    if (options.contains('help)) {
      println(usage)
      sys.exit(1)
    }

    val verbosity = options.getOrElse('verbosity, "1").asInstanceOf[Int]
    val aligner = new Aligner(System.in,
                              System.out,
                              System.err,
                              useAligner3 = !options.contains('aligner1),
                              verbosity,
                              options)
    aligner.run()
  }

}

