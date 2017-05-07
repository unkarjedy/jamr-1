package edu.cmu.lti.nlp.amr

import java.io.{InputStream, PrintStream}
import java.util.Properties

import edu.stanford.nlp.ling.CoreAnnotations._
import edu.stanford.nlp.pipeline.StanfordCoreNLP
import edu.stanford.nlp.trees.TreeCoreAnnotations.TreeAnnotation
import edu.stanford.nlp.trees._
import edu.stanford.nlp.util.{CoreMap, Filters}

import scala.collection.JavaConversions._


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
class StanfordProcessor {
  private val processor: StanfordCoreNLP = {
    val props = new Properties()
    props.setProperty("annotators", "tokenize,ssplit,parse")
    props.setProperty("parse.model", "edu/stanford/nlp/models/lexparser/englishRNN.ser.gz")
    new StanfordCoreNLP(props)
  }

  // don't filter out punctuation dependencies
  private val grammaticalStructureFactory = new EnglishGrammaticalStructureFactory(Filters.acceptFilter())

  /**
    * Parse to basic dependencies in conllx format.
    * Undoes any changes Stanford makes to the word forms.
    */
  def parse(input: String): List[List[ConllToken]] = {
    val annotation = processor.process(input)
    val sentences: List[CoreMap] = annotation.get(classOf[SentencesAnnotation]).toList

    for (sentence <- sentences) yield {
      val tree = sentence.get(classOf[TreeAnnotation])
      val grammaticalStructure = grammaticalStructureFactory.newGrammaticalStructure(tree)
      val dependencies = grammaticalStructure.typedDependencies().toList.sortBy(_.dep.index)
      val tokens = sentence.get(classOf[TokensAnnotation]).toList

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
  }

  def parseToConll(input: String): String = {
    parse(input)
      .map(_.mkString("\n"))
      .mkString("\n\n")
  }
}

class RunStanfordParser(in: InputStream, out: PrintStream) extends Runnable {
  override def run(): Unit = {
    val processor = new StanfordProcessor

    for (sentence <- Source.fromInputStream(in).getLines()) {
      val result = processor
        .parseToConll(sentence)
        .replaceAllLiterally("\n\n", "\n")
      out.println(s"$result\n")
    }
  }
}

object RunStanfordParser extends App {
  val parser = new RunStanfordParser(System.in, System.out)
  parser.run()
}
