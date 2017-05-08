package term_dict_process

import java.io.FileReader

import org.apache.commons.csv.{CSVFormat, CSVParser}
import org.apache.commons.lang3.StringUtils
import scripts.train.RunProperties

import scala.collection.JavaConversions._
import scala.collection.mutable

case class Term(id: Int, value: String,
                definitions: mutable.ArrayBuffer[TermDefinition] = mutable.ArrayBuffer(),
                synonyms: mutable.ArrayBuffer[String] = mutable.ArrayBuffer())

case class TermDefinition(id: Int, termId: Int, value: String,
                          sentences: mutable.ArrayBuffer[String] = mutable.ArrayBuffer())


object TermsDefinitionsProcessor {
  val CSV_FORMAT = CSVFormat.MYSQL.withQuote(''').withDelimiter(',')
  val runProperties = new RunProperties("run.properties")

  val jamrRoot = runProperties.jamrRoot
  val baseFolder = s"$jamrRoot/data/terms/"
  val resoursesBasePath = s"$jamrRoot/resources_terms"
  val termsFile = "terms_comp.txt"
  val termsDefinitionsFile = "terms_comp_definition.txt"

  def main(args: Array[String]): Unit = {
    val terms = loadTerms(s"$baseFolder/$termsFile").sortBy(_.value)
    val definitions = loadDefinitions(s"$baseFolder/$termsDefinitionsFile")

    initTermDefinitionsReferences(terms, definitions)
    splitDefinitionsToSentences(terms, definitions)
    extractSynonyms(terms)

    val saver = new TermDefinitionSaver(resoursesBasePath)
//    saver.saveTerms(terms, "terms.txt")
//    saver.saveTermsLowercased(terms, "terms-lowercased.txt")
//    saver.saveTermsWithSynonims(terms, "terms_with_synonims.txt")
//    saver.saveTermsWithDefinitionsToFile(terms, "term-definitions.txt")
//    saver.saveDefinitionsSentencesToFile(terms, "definition-sentences.txt")
//    saver.saveDefinitionsFirstSentencesToFile(terms, "definition-first-sentences.txt", splitDefinitions = true)
//    saver.saveDefinitionsFirstSentencesToFile(terms, "definition-first-sentences-no-blank.txt")

    val allSentencesLowercased = terms.flatMap(_.definitions).flatMap(_.sentences).map(_.toLowerCase)
    val counter = new NGrammCounter(allSentencesLowercased)
    val bigramsCountSorted = counter.countBigrams()
      .sortBy(_._2)(Ordering[Int].reverse)
      .take(1500)
    saver.saveTuples(bigramsCountSorted, "bigrams_frequency_sorted.txt")
  }

  /** The method extracts synonyms from first sentence of each definition.
    * Example: UBL - "(Universal Business Language) A format for exchanging data..."
    * The synonym inside the brackets is then removed out of the definition */
  def extractSynonyms(terms: mutable.Seq[Term]) = {
    for (term <- terms;
         definition <- term.definitions if definition.sentences.nonEmpty) {
      val firstSentence = definition.sentences.head
      val reg = "^\\s*\\((.*?)\\)(.*)$".r // ex: (Universal Business Language) A format for exchanging data...
      firstSentence match {
        case reg(synonim, other) =>
          term.synonyms ++= synonim.split(",").map(_.trim)
          definition.sentences(0) = other.trim
        case _ =>
      }
    }
  }

  def initTermDefinitionsReferences(terms: mutable.Seq[Term], definitions: mutable.Seq[TermDefinition]) = {
    val idToTemr = terms.map(t => t.id -> t).toMap // index for fast access by id
    definitions.foreach(d => {
      idToTemr(d.termId).definitions += d
    })
  }

  def splitDefinitionsToSentences(terms: mutable.Seq[Term], definitions: mutable.Seq[TermDefinition]) = {
    val senteseSplitter = new SenteseSplitter(terms)
    definitions.foreach(d => d.sentences ++= senteseSplitter.split(d.value))
  }

  def loadTerms(filePath: String): mutable.Seq[Term] = {
    val fileReader = new FileReader(filePath)
    val parser = new CSVParser(fileReader, CSV_FORMAT)
    parser.getRecords.map(record => {
      Term(record.get(0).toInt, record.get(1).trim)
    })
  }

  def loadDefinitions(filePath: String): mutable.Seq[TermDefinition] = {
    val fileReader = new FileReader(filePath)
    val parser = new CSVParser(fileReader, CSV_FORMAT)
    parser.getRecords
      .map(record => TermDefinition(
        id = record.get(0).toInt,
        termId = record.get(1).toInt,
        value = record.get(2).trim
      ))
      .filter(t => StringUtils.isNotBlank(t.value))
      .filter(t => StringUtils.isAsciiPrintable(t.value))
  }
}
