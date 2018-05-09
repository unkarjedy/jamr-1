package term_dict_process

import java.io.{File, PrintStream}

import scala.collection.mutable

class TermDefinitionSaver(val baseFolder: String) {

  init()

  private def init(): Unit = {
    new File(baseFolder).mkdirs()
  }

  def saveSentences(sentences: Seq[String], fileName: String) = {
    val printStream = getNewPrintStream(fileName)
    sentences.foreach(printStream.println)
  }


  def saveTuples(tuples: Seq[_ <: Product], fileName: String): Unit = {
    val printStream = getNewPrintStream(fileName)
    tuples.foreach { t =>
      printStream.println(t.productIterator.mkString(" "))
    }
  }

  def saveTerms(terms: Seq[Term], fileName: String): Unit = {
    val printStream = getNewPrintStream(fileName)
    terms.foreach(t => printStream.println(t.value))
  }

  def saveTermsLowercased(terms: Seq[Term], fileName: String): Unit = {
    val printStream = getNewPrintStream(fileName)
    terms.foreach(t => printStream.println(t.value.toLowerCase()))
  }

  def saveTermsWithSynonims(terms: Seq[Term], fileName: String): Unit = {
    val printStream = getNewPrintStream(fileName)
    terms.foreach { t =>
      printStream.print(t.value)
      if (t.synonyms.nonEmpty) {
        printStream.print(t.synonyms.mkString(" ### ", " ### ", ""))
      }
      printStream.println()
    }
  }

  def saveTermsWithDefinitionsToFile(terms: Seq[Term], fileName: String) = {
    val printStream = getNewPrintStream(fileName)
    terms.filter(_.definitions.nonEmpty).foreach { term =>
      printStream.println(term.value)
      term.definitions.foreach(d => printStream.println(d.value))
      printStream.println()
    }
  }

  def saveTermsWithDefinitionSentencesToFile(termSentences: Seq[TermWithSentences], fileName: String) = {
    val printStream = getNewPrintStream(fileName)

    termSentences.filter(_.sentences.nonEmpty).foreach { case TermWithSentences(term, sentences) =>
      printStream.println(term)
      sentences.foreach(s => printStream.println(s.value))
      printStream.println()
    }
  }

  def saveDefinitionsSentencesToFile(terms: Seq[Term], fileName: String): Unit = {
    val printStream = getNewPrintStream(fileName)
    terms.filter(_.definitions.nonEmpty).foreach { term =>
      term.definitions.flatMap(_.sentences).foreach(s => {
        printStream.println(s)
      })
      printStream.println()
    }
  }

  def saveDefinitionsFirstSentencesToFile(terms: Seq[Term], fileName: String,
                                          splitDefinitionsWithNewLine: Boolean = false): Unit = {
    val printStream = getNewPrintStream(fileName)
    terms.filter(_.definitions.nonEmpty).foreach { term =>
      term.definitions.flatMap(_.sentences.headOption).foreach(s => {
        printStream.println(s)
      })
      if (splitDefinitionsWithNewLine) {
        printStream.println()
      }
    }
  }

  def saveCustom[T](elements: Seq[T], fileName: String)(transform: T => String): Unit = {
    Utils.using(getNewPrintStream(fileName)) { printStream =>
      elements.foreach { element =>
        printStream.println(transform(element))
      }
    }
  }

  private def calcFullPath(path: String): String = {
    s"$baseFolder/$path"
  }

  private def getNewPrintStream(path: String): PrintStream = {
    new PrintStream(calcFullPath(path))
  }
}
