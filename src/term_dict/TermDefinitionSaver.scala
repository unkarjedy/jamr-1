package term_dict

import java.io.PrintStream

import scala.collection.mutable


class TermDefinitionSaver(baseFolder: String) {

  private def calcFullPath(path: String) : String= {
    s"$baseFolder/$path"
  }

  private def getNewPrintStream(path: String): PrintStream = {
    new PrintStream(calcFullPath(path))
  }

  def saveTerms(terms: mutable.Seq[Term], filePath: String): Unit = {
    val printStream = getNewPrintStream(filePath)
    terms.foreach(t => printStream.println(t.value))
  }

  def saveTermsLowercased(terms: mutable.Seq[Term], filePath: String): Unit = {
    val printStream = getNewPrintStream(filePath)
    terms.foreach(t => printStream.println(t.value.toLowerCase()))
  }

  def saveTermsWithSynonims(terms: mutable.Seq[Term], filePath: String): Unit = {
    val printStream = getNewPrintStream(filePath)
    terms.foreach(t => {
      printStream.print(t.value)
      if (t.synonyms.nonEmpty) {
        printStream.print(t.synonyms.mkString(" ### ", " ### ", ""))
      }
      printStream.println()
    })
  }

  def saveTermsWithDefinitionsToFile(terms: mutable.Seq[Term], filePath: String) = {
    val printStream = getNewPrintStream(filePath)
    terms.filter(_.definitions.nonEmpty).foreach(term => {
      printStream.println(term.value)
      term.definitions.foreach(d => printStream.println(d.value))
      printStream.println()
    })
  }

  def saveDefinitionsSentencesToFile(terms: mutable.Seq[Term], filePath: String): Unit = {
    val printStream = getNewPrintStream(filePath)
    terms.filter(_.definitions.nonEmpty).foreach(term => {
      term.definitions.flatMap(_.sentences).foreach(s => {
        printStream.println(s)
      })
      printStream.println()
    })
  }

  def saveDefinitionsFirstSentencesToFile(terms: mutable.Seq[Term], filePath: String,
                                          splitDefinitions: Boolean = false): Unit = {
    val printStream = getNewPrintStream(filePath)
    terms.filter(_.definitions.nonEmpty).foreach(term => {
      term.definitions.flatMap(_.sentences.headOption).foreach(s => {
        printStream.println(s)
      })
      if(splitDefinitions){
        printStream.println()
      }
    })
  }
}
