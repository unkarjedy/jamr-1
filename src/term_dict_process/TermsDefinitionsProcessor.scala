package term_dict_process

import java.io.{File, FileReader}

import com.typesafe.scalalogging.slf4j.Logging
import org.apache.commons.csv.{CSVFormat, CSVParser}
import org.apache.commons.lang3.StringUtils
import scripts.train.RunProperties
import scripts.utils.FileExt._

import scala.collection.JavaConversions._
import scala.collection.mutable
import scala.io.Source

/**
  * This util class is used to process `terms` files, extracted from MySQL database given by Ivankov AA.
  * Parse that files, extract some useful information and save it to different files
  */
object TermsDefinitionsProcessor extends Logging {
  val CSV_FORMAT = CSVFormat.MYSQL.withQuote(''').withDelimiter(',')
  val runProperties = new RunProperties("run.properties")

  val jamrRoot = runProperties.jamrRoot
  val baseFolder = s"$jamrRoot/data/terms/"
  val resourcesBasePath = s"$jamrRoot/resources_terms"
  val termsFile = "terms_comp.txt"
  val termsDefinitionsFile = "terms_comp_definition.txt"

  def main(args: Array[String]): Unit = {
    logger.debug("Loading terms")
    val terms = loadTerms(s"$baseFolder/$termsFile").sortBy(_.value)
    logger.debug("Loading terms definitions")
    val definitions: mutable.Seq[TermDefinition] =
      loadDefinitions(s"$baseFolder/$termsDefinitionsFile")

    logger.debug("Initializing terms & definitions references")
    initTermDefinitionsReferences(terms, definitions)
    logger.debug("Splitting definitions to sentences")
    splitDefinitionsToSentences(terms, definitions)
    logger.debug("Searching for synonyms")
    extractSynonyms(terms)
    logger.debug("Clear definitions sentences")
    clearDefinitionsSentences(definitions)

    logger.debug("Saving stuff...")
    val saver = new TermDefinitionSaver(resourcesBasePath)

    def saveStuff(): Unit = {
      saver.saveTerms(terms, "terms.txt")
      saver.saveTermsLowercased(terms, "terms-lowercased.txt")
      saver.saveTermsWithSynonims(terms, "terms_with_synonims.txt")
      saver.saveTermsWithDefinitionsToFile(terms, "term-definitions.txt")
      saver.saveDefinitionsSentencesToFile(terms, "definition-sentences.txt")
      saver.saveDefinitionsFirstSentencesToFile(terms, "definition-first-sentences.txt", splitDefinitionsWithNewLine = true)
      saver.saveDefinitionsFirstSentencesToFile(terms, "definition-first-sentences-no-blank.txt")
    }

    // saveStuff()

    def isDeterminer(sentence: String): Boolean = {
      StringUtils.startsWithIgnoreCase(sentence, "a ") ||
        StringUtils.startsWithIgnoreCase(sentence, "an ")
    }

    {
      logger.debug("Calculating terms usages")

      val MostUsedTermsFileName = "most_used_terms.txt"

      def calcMostUsedAndSave(): Seq[TermUsages] = {
        val x = calcTermsUsages(terms).sortBy(-_.usages)
        saver.saveCustom(x, MostUsedTermsFileName) { mu =>
          s"${mu.usages}\t${mu.term}"
        }
        x
      }

      val termByLoweredValue: Map[String, Term] = {
        val x = terms.groupBy(_.value.toLowerCase)
        x.values.find(_.size != 1) match {
          case Some(t) =>
            assert(assertion = false, s"$t. Terms should be unique comparing by lowercase!")
          case _ =>
        }
        x.mapValues(_.head)
      }

      def readMostUsed(): Seq[TermUsages] = {
        val reg = """(\d+)\t(.*)""".r
        val file = new File(saver.baseFolder).resolve(MostUsedTermsFileName)
        Source.fromFile(file).getLines()
          .map { case reg(used, term) => TermUsages(used.toInt, term) }
          .toArray
          .toSeq
      }

      val mostUsed: Seq[TermUsages] = readMostUsed() // calcMostUsedAndSave()

      val termsWithSentences: Seq[TermWithSentences] = mostUsed
        .toStream
        .flatMap { tu =>
          val termLower = tu.term.toLowerCase()
          val term: Term = termByLoweredValue(termLower)
          term.definitions.map(d => TermWithSentences(termLower, d.sentences))
        }
        .map { ts =>
          val sentences = ts.sentences
            // .filterNot(_.value.contains("\""))
            .map(_.updated(_.replaceFirst("^Technically , ", "")))
            .take(5)
          ts.copy(sentences = sentences)
        }
        .filter(_.sentences.nonEmpty)
      saver.saveTermsWithDefinitionSentencesToFile(termsWithSentences, "most_used_term_defs.txt")

      mostUsed
    }

    def buildDictSentencesAndSave1(): Unit = {
      val firstDetSentences: Seq[String] = definitions
        .flatMap(_.sentences.headOption.map(_.value))
        .filter(isDeterminer)

      val N = 100 // we need to take N sentences, uniformly from all sentencesDeterminers
      val NSentences: Seq[String] = firstDetSentences
        .grouped(firstDetSentences.length / N)
        .map(_.head)
        .toSeq

      val outputSentences = firstDetSentences // NSentences
      saver.saveSentences(outputSentences, s"sentences${outputSentences.length}.txt")
    }

    // buildDictSentencesAndSave1()

    def countBigramsAndSave(): Unit = {
      val allSentencesLowerCased = terms.flatMap(_.definitions).flatMap(_.sentences).map(_.value.toLowerCase)
      val counter = new NGrammCounter(allSentencesLowerCased)
      val bigramsCountSorted = counter.countBigrams()
        .sortBy(_._2)(Ordering[Int].reverse)
        .take(1500)
      saver.saveTuples(bigramsCountSorted, "bigrams_frequency_sorted.txt")
    }

    //justTest_BigramCounter()
  }

  /** The method extracts synonyms from first sentence of each definition.
    * Example: UBL - "(Universal Business Language) A format for exchanging data..."
    * The synonym inside the brackets is then removed out of the definition */
  def extractSynonyms(terms: mutable.Seq[Term]): Unit = {
    for (term <- terms;
         definition <- term.definitions if definition.sentences.nonEmpty) {
      val firstSentence = definition.sentences.head.value
      val reg = "^\\s*\\((.*?)\\)(.*)$".r // ex: (Universal Business Language) A format for exchanging data...
      firstSentence match {
        case reg(synonim, other) =>
          term.synonyms ++= synonim.split(",").map(_.trim)
          definition.sentences(0) = Sentence(other.trim)
        case _ =>
      }
    }
  }

  def initTermDefinitionsReferences(terms: mutable.Seq[Term], definitions: mutable.Seq[TermDefinition]): Unit = {
    val idToTerm: Map[Int, Term] = terms.map(t => t.id -> t).toMap // index for fast access by id
    definitions.foreach(d => {
      idToTerm(d.termId).definitions += d
    })
  }

  def splitDefinitionsToSentences(terms: mutable.Seq[Term], definitions: mutable.Seq[TermDefinition]) = {
    val senteceSplitter = new SenteseSplitter(terms)
    definitions.foreach { case defin@TermDefinition(_, _, _, _) =>
      val sentences = senteceSplitter.split(defin.value)
      if (areOk(sentences)) {
        defin.sentences ++= sentences.map(Sentence.apply)
      }
    }
  }

  private def areOk(senteces: Seq[String]): Boolean = {
    // if first sentence does not contain any quote
    !senteces.headOption.exists(_.contains('"'))
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

  private def clearDefinitionsSentences(definitions: mutable.Seq[TermDefinition]): Unit = {
    definitions.foreach { d =>
      d.sentences = d.sentences.map(_.value).map(
        (identity[String] _)
          .andThen(clearNerInc)
          .andThen(removeBracketsContent)
          .andThen(fixPunctuation)
          .andThen(clearWhiteSpaces)
      ).map(Sentence.apply)
    }
  }

  private def clearNerInc(str: String): String =
    str.replaceAll("""(?i)\,\s*inc""", " Inc")

  private def removeBracketsContent(str: String): String =
    str.replaceAll("""\s*\(.*?\)\s*""", " ")

  private def clearWhiteSpaces(str: String): String = {
    val Space = " "
    str.replaceAll("""\s+""", Space)
  }

  // punctuation should be surrounded with spaces for convinient`contains` lookup
  private def fixPunctuation(str: String): String = {
    val result = str.replaceAll("""\s*([\,\:\.\"\&\(\)\!\?])\s*""", " $1 ")
    result
  }

  // This method is written fast, no effort to achive thread parallelism was applied
  // NOTE! This method implies that punctuation was applied to all definitions
  private def calcTermsUsages(terms: Seq[Term]): Seq[TermUsages] = {
    val termLowerToTermUsage: Map[String, TermUsagesMutable] = {
      terms.map(term => {
        val termLower = term.value.toLowerCase
        termLower -> TermUsagesMutable(0, termLower)
      }).toMap
    }

    def tokenize(str: String): Seq[String] = str.split("\\s")

    val IngoreTerms: Set[String] =
      Set("IN", "IS", "OR", "AS", "Be", "IT", "WAS", "CAN", "ITS", "AT", "NOT", "up", "&")
        .map(_.toLowerCase)

    // !!! O(m * n^2) where n = |terms|, m = max(|sentences|)
    for {
      (term1@Term(_, _, definitions, _), idx) <- terms.zipWithIndex.toStream
      _ = {
        if (idx % (terms.size / 100) == 0) logger.debug(s"${(100 * idx / terms.size)}% ($idx / ${terms.size})")
        else ()
      }
      definition <- definitions
      sent <- definition.sentences
      (tokens: Set[String]) = tokenize(sent.value).map(_.toLowerCase).toSet
      term2 <- terms if {
        val termLower = term2.value.toLowerCase
        term2.value != term1.value &&
          tokens.contains(termLower) &&
          !IngoreTerms.contains(termLower) &&
          !termLower.forall(_.isDigit)
      }
    } {
      val termLower = term2.value.toLowerCase
      // val occurences = tokens.count(_ == termLower) // NOTE: tokens should not be Set here if you want to uncomment
      termLowerToTermUsage(termLower).usages += 1

      sent.termOccurrences(termLower) = sent.termOccurrences(termLower) + 1
    }

    termLowerToTermUsage.values.map(_.toImmutable).toSeq
  }
}