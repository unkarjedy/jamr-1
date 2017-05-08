package edu.cmu.lti.nlp.amr.term

import edu.cmu.lti.nlp.amr.term.TermsDict._
import org.apache.commons.lang3.StringUtils

import scala.collection.mutable
import scala.io.Source

case class Term(words: Seq[String], concept: String, onlyUpperCase: Boolean = false)

class TermsDict(filePathOpt: Option[String]){
  private var wordToTerm = mutable.Map[String, List[Term]]()

  def getOrElse(firstWord: String, default: => List[Term]): List[Term] = {
    wordToTerm.getOrElse(firstWord, default)
  }

  def get(firstWord: String): Option[List[Term]] = {
    wordToTerm.get(firstWord)
  }

  filePathOpt.foreach(termsFile => {
    Source.fromFile(termsFile)
      .getLines()
      .map(_.trim)
      .foreach(line => {
        val parts = line.split(" ### ")
        val termValue = parts.head.trim
        val termSynonyms = parts.tail
        val termWords = line.split("\\s+")
        val concept = termWords.mkString("-") + TERM_CONCEPT_SUFFIX
        val term = Term(termWords, concept, onlyUpperCase = TermsDict.isAbviature(termValue, termSynonyms))
        wordToTerm(termWords.head) = term :: wordToTerm.getOrElse(termWords.head, List())
      })
  })
}

object TermsDict {
  val TERM_CONCEPT_SUFFIX = "-777"

  private def isAbviature(term: String, synonyms: Array[String]): Boolean = {
    val termLetters = term.filter(_.isLetter)
    StringUtils.isAllUpperCase(termLetters ) && synonyms.exists(isAbviature(termLetters , _))
  }

  private def isAbviature(term: String, synonym: String): Boolean = {
    val termLettersStr = term.map(_.toLower)
    val synonimFirstLetters = StringUtils.split(synonym).map(_.head).map(_.toLower).mkString("")
    termLettersStr == synonimFirstLetters
  }

  def main(args: Array[String]): Unit = {
    testIsAbviature()
  }

  // of cause better add JUnit to the project... but no time..
  def testIsAbviature(): Unit = {
    assert(isAbviature("US", Array("United States")))
    assert(isAbviature("US", Array("  united   states  ")))
    assert(isAbviature("U.S.", Array("United States")))
    assert(isAbviature("U..S..", Array("United States")))

    assert(!isAbviature("US", Array("United States of Rusia")))
    assert(!isAbviature("US", Array("United Russia States")))
    assert(!isAbviature("U.s", Array("United States")))
    assert(!isAbviature("u", Array("United States")))
  }

}