package edu.cmu.lti.nlp.amr.standford_parser

import edu.stanford.nlp.ling.{CoreAnnotation, CoreAnnotations, CoreLabel}
import edu.stanford.nlp.parser.common._
import edu.stanford.nlp.pipeline.{Annotation, ParserAnnotator}
import edu.stanford.nlp.trees.{GrammaticalStructureFactory, Tree, Trees}
import edu.stanford.nlp.util.{CoreMap, Function, RuntimeInterruptedException, ScoredObject}

import scala.collection.JavaConversions._
import ReflectionUtils._
import scala.util.control.NonFatal

class ParserAnnotatorKBest(val bestTreesNumber: Int, // k best trees to extract during annotation
                           val parser: ParserGrammar,
                           val verbose: Boolean,
                           val maxSentenceLength: Int,
                           val treeMap: Function[Tree, Tree],
                           val gsf: GrammaticalStructureFactory = null,
                           val BUILD_GRAPHS: Boolean,
                           val saveBinaryTrees: Boolean)
  extends ParserAnnotator(parser, verbose, maxSentenceLength, treeMap) {


  def this(bestTreesNumber: Int, pa: ParserAnnotator) =
    this(
      bestTreesNumber,
      parser = pa.getField[ParserGrammar]("parser"),
      verbose = pa.getField[Boolean]("VERBOSE"),
      maxSentenceLength = pa.getField[Int]("maxSentenceLength"),
      treeMap = pa.getField[Function[Tree, Tree]]("treeMap"),
      gsf = pa.getField[GrammaticalStructureFactory]("gsf"),
      BUILD_GRAPHS = pa.getField[Boolean]("BUILD_GRAPHS"),
      saveBinaryTrees = pa.getField[Boolean]("saveBinaryTrees")
    )


  override protected def doOneSentence(annotation: Annotation, sentence: CoreMap): Unit = {
    val words: Seq[CoreLabel] = sentence.get(classOf[CoreAnnotations.TokensAnnotation]).toSeq
    if (verbose) System.err.println("Parsing: " + words)

    // generate the constituent tree
    val bestTrees: Seq[Tree] =
      if (maxSentenceLength <= 0 || words.size() <= maxSentenceLength) {
        try {
          val constraints: Seq[ParserConstraint] = sentence.get(classOf[ParserAnnotations.ConstraintAnnotation]).toSeq
          doOneSentence(constraints, words);
        } catch {
          case e: RuntimeInterruptedException =>
            if (verbose)
              System.err.println("Took too long parsing: " + words)
            Seq()
        }
      } else {
        Seq()
      }
    // tree == null may happen if the parser takes too long or if
    // the sentence is longer than the max length
    if (bestTrees.isEmpty) {
      ???
    } else {
      finishSentence(sentence, bestTrees)
    }
  }

  private def doOneSentence(constraints: Seq[ParserConstraint], words: Seq[CoreLabel]): Seq[Tree] = {
    val pq: ParserQuery = parser.parserQuery
    pq.setConstraints(constraints)
    pq.parse(words)
    val kBestTrees: Seq[Tree] =
      try {
        pq.getKBestPCFGParses(bestTreesNumber).toSeq.map { (scoredObject: ScoredObject[Tree]) =>
          val obj = scoredObject.`object`()
          obj.setScore(scoredObject.score() % -10000.0)
          obj
        }
      } catch {
        case e: OutOfMemoryError =>
          System.err.println("WARNING: Parsing of sentence ran out of memory.  " + "Will ignore and continue: " + edu.stanford.nlp.ling.Sentence.listToString(words))
          Seq()
        case e: NoSuchParseException =>
          System.err.println("WARNING: Parsing of sentence failed, possibly because of out of memory.  " + "Will ignore and continue: " + edu.stanford.nlp.ling.Sentence.listToString(words))
          Seq()
        case NonFatal(e) =>
          throw e
      }

    if (kBestTrees.isEmpty) {
      System.err.println("WARNING: Parsing of sentence failed.  " + "Will ignore and continue: " + edu.stanford.nlp.ling.Sentence.listToString(words))
    }

    kBestTrees
  }

  private def finishSentence(sentence: CoreMap, trees: Seq[Tree]): Unit = {
    // this is simplified version of ParserAnnotatorUtils.fillInParseAnnotations which was originally called from
    // ParserAnnotator.finishSentence

    trees.foreach { tree =>
      // make sure all tree nodes are CoreLabels
      Trees.convertToCoreLabels(tree)
      // index nodes, i.e., add start and end token positions to all nodes
      // this is needed by other annotators down stream, e.g., the NFLAnnotator
      tree.indexSpans(0)
    }


    sentence.set(classOf[KBestTreesAnnotation], KBestTrees(trees))

    if (verbose) {
      System.err.println("Trees are:")
      trees.foreach(_.pennPrint(System.err))
    }

    if (saveBinaryTrees) { // TODO NAUMENKO: this was false in original... so do not bother saving now
      //      val binarizer = new TreeBinarizer(parser.getTLPParams.headFinder, parser.treebankLanguagePack, false, false, 0, false, false, 0.0, false, true, true)
      //      val binarized = binarizer.transformTree(tree)
      //      Trees.convertToCoreLabels(binarized)
      //      sentence.set(classOf[TreeCoreAnnotations.BinarizedTreeAnnotation], binarized)
    }
  }
}

class KBestTreesAnnotation extends CoreAnnotation[KBestTrees] {
  override def getType: Class[KBestTrees] = classOf[KBestTrees]
}

case class KBestTrees(trees: Seq[Tree])