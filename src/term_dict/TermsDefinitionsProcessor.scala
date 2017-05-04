package term_dict

import java.io.{FileReader, PrintStream}

import org.apache.commons.csv.{CSVFormat, CSVParser}
import org.apache.commons.lang3.StringUtils
import scripts.train.RunProperties

import scala.collection.JavaConversions._
import scala.collection.mutable

case class Term(id: Int, value: String,
                // 1 term to many definitions
                definitions: mutable.ArrayBuffer[TermDefinition] = mutable.ArrayBuffer(),
                synonyms: mutable.ArrayBuffer[String] = mutable.ArrayBuffer())

case class TermDefinition(id: Int, termId: Int, value: String,
                          sentences: mutable.ArrayBuffer[String] = mutable.ArrayBuffer())


object TermsDefinitionsProcessor {
  val CSV_FORMAT = CSVFormat.MYSQL.withQuote(''').withDelimiter(',')
  val runProperties = new RunProperties("run.properties")

  val jamrRoot = runProperties.jamrRoot
  val baseFolder = s"$jamrRoot/data/terms/"
  val resoursesBasePath = s"$jamrRoot/resources"
  val termsFile = "terms_comp.txt"
  val termsDefinitionsFile = "terms_comp_definition.txt"

  def main(args: Array[String]): Unit = {
    val terms = loadTerms(s"$baseFolder/$termsFile").sortBy(_.value)
    val definitions = loadDefinitions(s"$baseFolder/$termsDefinitionsFile")

    initTermDefinitionsReferences(terms, definitions)
    parseDefinitionsSentences(terms, definitions)
    extractSynonyms(terms)

    saveTerms(terms, s"$resoursesBasePath/terms.txt")
    saveTermsWithSynonims(terms, s"$resoursesBasePath/terms_with_synonims.txt")
    saveTermsWithDefinitionsToFile(terms, s"$resoursesBasePath/term-definitions.txt")
    saveDefinitionsSentencesToFile(terms, s"$resoursesBasePath/definition-sentences.txt")
  }

  /** The method extracts synonyms from first sentence of each definition.
    * Example: UBL - (Universal Business Language) A format for exchanging data
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
    // build indexes for fast access by field
    val idToTemr = terms.map(t => t.id -> t).toMap
    // load definitions into terms
    definitions.foreach(d => {
      idToTemr(d.termId).definitions += d
    })
  }

  def parseDefinitionsSentences(terms: mutable.Seq[Term], definitions: mutable.Seq[TermDefinition]) = {
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


  def saveTerms(terms: mutable.Seq[Term], filePath: String): Unit = {
    val printStream = new PrintStream(filePath)
    terms.foreach(t => printStream.println(t.value))
  }

  def saveTermsWithSynonims(terms: mutable.Seq[Term], filePath: String): Unit = {
    val printStream = new PrintStream(filePath)
    terms.foreach(t => {
      printStream.print(t.value)
      if (t.synonyms.nonEmpty) {
        printStream.print(t.synonyms.mkString(" ### ", " ### ", ""))
      }
      printStream.println()
    })
  }

  def saveTermsWithDefinitionsToFile(terms: mutable.Seq[Term], filePath: String) = {
    val printStream = new PrintStream(filePath)
    terms.filter(_.definitions.nonEmpty).foreach(term => {
      printStream.println(term.value)
      term.definitions.foreach(d => printStream.println(d.value))
      printStream.println()
    })
  }

  def saveDefinitionsSentencesToFile(terms: mutable.Seq[Term], filePath: String): Unit = {
    val printStream = new PrintStream(filePath)
    terms.filter(_.definitions.nonEmpty).foreach(term => {
      term.definitions.flatMap(_.sentences).foreach(s => {
        printStream.println(s)
      })
      printStream.println()
    })
  }

}
