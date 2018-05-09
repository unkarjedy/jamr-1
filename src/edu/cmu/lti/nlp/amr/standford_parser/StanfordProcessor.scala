package edu.cmu.lti.nlp.amr.standford_parser

import java.io.{InputStream, PrintStream}
import java.util.{Properties, List => JList}

import edu.cmu.lti.nlp.amr.Source
import edu.cmu.lti.nlp.amr.standford_parser.ReflectionUtils._
import edu.stanford.nlp.ling.CoreAnnotations._
import edu.stanford.nlp.ling.CoreLabel
import edu.stanford.nlp.parser.lexparser.LexicalizedParser
import edu.stanford.nlp.pipeline.{Annotator, ParserAnnotator, StanfordCoreNLP, TokenizerAnnotator, WordsToSentencesAnnotator, Annotation => SFAnnotation}
import edu.stanford.nlp.trees.TreeCoreAnnotations.TreeAnnotation
import edu.stanford.nlp.trees._
import edu.stanford.nlp.util.{CoreMap, Filters}
import scripts.parse.InputSentencesReader
import scripts.utils.logger.SimpleLoggerLike

import scala.collection.JavaConversions._
import scala.collection.JavaConverters._


/*
 * Description of Conll format:
 * http://ufal.mff.cuni.cz/conll2009-st/task-description.html
 * http://universaldependencies.org/format.html
 *
 * _ correspons to None
 */
case class ConllToken(index: Option[Int],
                      form: Option[String],
                      lemma: Option[String],
                      pos: Option[String],
                      cpos: Option[String],
                      feats: Option[String],
                      gov: Option[Int],
                      deprel: Option[String],
                      phead: Option[Int],
                      pdeprel: Option[String]) extends Iterable[Option[_]] {

  override def toString(): String = {
    map(_.getOrElse("_")).mkString("\t")
  }

  override def iterator: Iterator[Option[_]] = {
    productIterator.asInstanceOf[Iterator[Option[_]]]
  }
}

/**
  * Thin wrapper around edu.stanford.nlp.pipeline.StanfordCoreNLP
  *
  * @author sthomson@cs.cmu.edu
  */
class StanfordProcessor(verbose: Boolean = false) extends SimpleLoggerLike {

  private val coreNlpPipeline: StanfordCoreNLP = {
    val props = new Properties()
    props.setProperty("annotators", "tokenize,ssplit,parse") // Consider `ner,dcoref` properties?
    props.setProperty("parse.model", "edu/stanford/nlp/models/lexparser/englishRNN.ser.gz")
    // props.setProperty("parse.model", "edu/stanford/nlp/models/lexparser/englishPCFG.ser.gz")
    new StanfordCoreNLP(props)
  }

  // don't filter out punctuation dependencies
  private val grammaticalStructureFactory = new EnglishGrammaticalStructureFactory(Filters.acceptFilter())

  // coreNlpPipeline.process(input) // this was original line.. just one line =)
  private def annotate(input: String): SFAnnotation = {
    val annotation = new SFAnnotation(input)
    val annotators = coreNlpPipeline.getField[JList[Annotator]]("annotators").asScala

    // annotators corresponding to tokenize,ssplit,parse
    val tokenizerAnnotator = annotators.collect { case x: TokenizerAnnotator => x }.head
    val splitAnnotator = annotators.collect { case x: WordsToSentencesAnnotator => x }.head
    val parserAnnotator = annotators.collect { case x: ParserAnnotator => x }.head
    val parserKBestAnnotator = new ParserAnnotatorKBest(bestTreesNumber = 20, parserAnnotator)

    val parser: LexicalizedParser = parserAnnotator.getField[LexicalizedParser]("parser")
    parser.getOp.testOptions.maxSpanForTags = 1

    tokenizerAnnotator.annotate(annotation)
    splitAnnotator.annotate(annotation)
    parserAnnotator.annotate(annotation)
    parserKBestAnnotator.annotate(annotation)
    //    val sentence: CoreMap = annotation.get(classOf[CoreAnnotations.SentencesAnnotation]).head // assume that we have one sentence in input
    //    val words: Seq[CoreLabel] = sentence.get(classOf[CoreAnnotations.TokensAnnotation])
    //    val constraints: Seq[ParserConstraint] = sentence.get(classOf[ParserAnnotations.ConstraintAnnotation])
    //    val tree = doOneSentence(constraints, words)

    annotation
  }

  /**
    * Parse to basic dependencies in conllx format.
    * Undoes any changes Stanford makes to the word forms.
    */
  def parse(input: String): Seq[SentenceSingleParseResult] = {
    val annotation = annotate(input)
    val sentences: Seq[CoreMap] = annotation.get(classOf[SentencesAnnotation])

    for ((sentence, idx) <- sentences.zipWithIndex) yield {
      //      if (verbose) {
      //        logger.info(s"parsing sentence $idx")
      //      }
      val tree: Tree = sentence.get(classOf[TreeAnnotation])
      val tokens: Seq[ConllToken] = buildConllTokens(input, sentence, tree)
      SentenceSingleParseResult(sentence, idx, ConllTokensList(tokens))
    }
  }

  def parseKBest(input: String): Seq[SentenceKBestParseResults] = {
    val annotation = annotate(input)
    val sentences: Seq[CoreMap] = annotation.get(classOf[SentencesAnnotation])

    for ((sentence, idx) <- sentences.zipWithIndex) yield {
      val trees: Seq[Tree] = sentence.get(classOf[KBestTreesAnnotation]).trees

      val KBestDistinctTreesNumber = 5
      val kBestUniqueParses: Seq[ConllTokensList] =
        trees
          .map(t => buildConllTokens(input, sentence, t))
          .map(ConllTokensList)
          .distinct
          .take(KBestDistinctTreesNumber)

      (KBestDistinctTreesNumber - kBestUniqueParses.size) match {
        case 0 =>
        case n => logger.warning(s"duplicates among k-best trees found: $n of $KBestDistinctTreesNumber")
      }
      SentenceKBestParseResults(sentence, idx, kBestUniqueParses)
    }
  }

  private def buildConllTokens(input: String, sentence: CoreMap, tree: Tree): Seq[ConllToken] = {
    val grammaticalStructure: EnglishGrammaticalStructure = grammaticalStructureFactory.newGrammaticalStructure(tree)
    val dependencies: Seq[TypedDependency] = grammaticalStructure.typedDependencies().toList.sortBy(_.dep.index)
    val tokens: Seq[CoreLabel] = sentence.get(classOf[TokensAnnotation]).toList

    for ((token, dep) <- tokens.zip(dependencies)) yield {
      val start = token.get(classOf[CharacterOffsetBeginAnnotation])
      val end = token.get(classOf[CharacterOffsetEndAnnotation])
      val pos = token.get(classOf[PartOfSpeechAnnotation])
      
      ConllToken(
        index = Some(dep.dep.index),
        form = Some(input.substring(start, end)),
        lemma = None,
        pos = Some(pos),
        cpos = Some(pos),
        feats = None,
        gov = Some(dep.gov.index),
        deprel = Some(dep.reln.getShortName),
        phead = None,
        pdeprel = None
      )
    }
  }

  def parseToConllString(input: String): String = {
    parse(input)
      .map(_.conllTokens.tokens.mkString("\n"))
      .mkString("\n\n")
  }

  def parseToKBestConllString(input: String, inputIdx: Int): Seq[String] = {
//    if (verbose) {
//      logger.info(s"parsing sentence $inputIdx")
//    }
    // we expect that input contains only one sentence
    parseKBest(input).take(1).map { case SentenceKBestParseResults(sentence, sentenceIdx, bestConllTokens) =>
      val parseResultTextBlocks: Seq[String] = bestConllTokens.map(_.tokens.mkString("\n"))
      parseResultTextBlocks.zipWithIndex
        .map { case (textBlock, treeIdx) =>
          s"""# ::snt $sentence
             |# ::sntId $inputIdx
             |# ::treeId $treeIdx
             |$textBlock""".stripMargin
        }
        .mkString("\n\n")
    }
  }
}

case class ConllTokensList(tokens: Seq[ConllToken])
case class SentenceSingleParseResult(sentence: CoreMap, idx: Int, conllTokens: ConllTokensList)
case class SentenceKBestParseResults(sentence: CoreMap, idx: Int, conllTokens: Seq[ConllTokensList])

class RunStanfordParser(in: InputStream, out: PrintStream,
                        outKBestOpt: Option[PrintStream] = None,
                        verbose: Boolean = false) extends Runnable with SimpleLoggerLike {
  val SPECIAL_STOP_SENTENCE = "STOP_STOP_STOP"

  override def run(): Unit = {
    val processor = new StanfordProcessor(verbose)

    val lines = InputSentencesReader.getStream(Source.fromInputStream(in))
      .map(_.sentence)
      // .take(10)
      .takeWhile(_ != SPECIAL_STOP_SENTENCE)

    for ((sentence, sntIdx) <- lines.zipWithIndex) {
      if (verbose) {
        logger.info(s"parsing sentence $sntIdx")
      }
      val result = processor
        .parseToConllString(sentence)
        .replaceAllLiterally("\n\n", "\n")
      out.println(s"$result\n")

      outKBestOpt match {
        case Some(outKBest) =>
          processor.parseToKBestConllString(sentence, sntIdx).foreach(outKBest.println)
          outKBest.println()
        case None =>
      }
    }
  }
}

object RunStanfordParser {

  def main(args: Array[String]): Unit = {
    val parser = new RunStanfordParser(System.in, System.out)
    parser.run()
  }

}


/*


    val sentenceAnnotationOpt: Option[mutable.Buffer[CoreMap]] =
      Option(annotation.get(classOf[SentencesAnnotation])).map(_.asScala)

    def isFirstSentence =
      input.replaceAll("\\s+", "") == "An Internet domain name made up of standard ASCII characters".replaceAll("\\s+", "")

    sentenceAnnotationOpt match {
      case Some(sentenceAnnotation) if isFirstSentence && false =>
        sentenceAnnotation.map(_.asInstanceOf[SFAnnotation]).foreach { sentence: SFAnnotation =>
          // FIXME: just one pattern for 1st sentence: "An Internet domain name made up of standard ASCII characters"
          sentence.set(classOf[ParserAnnotations.ConstraintAnnotation], Seq(
            new ParserConstraint(1, 4, "NN|NN[^a-zA-Z].*")
          ).asJava)
        }
      case _ =>
    }
 */