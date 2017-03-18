package edu.cmu.lti.nlp.amr

import java.io.{InputStream, PrintStream}

import scala.collection.mutable.Map
import scala.io.Source.fromFile

object MakeConceptTable {
  def main(args: Array[String]): Unit = {
    val tokenizedSentences = args(0)
    val runner = MakeConceptTable(tokenizedSentences, System.in, System.out, System.err)
    runner.run()
  }
}

case class MakeConceptTable(inputTokenizeSentences: String,
                            in: InputStream,
                            out: PrintStream,
                            err: PrintStream) extends Runnable {
  override def run(): Unit = {
    val ConceptLineRegexp = """(.*) \|\|\| (.*) \|\|\| Count=(.*)""".r

    err.println("Reading in concept table")

    val phraseCounts: Map[String, Int] = Map()
    // first word in the concept phrase => list of (phrase, concept, count) triples
    val conceptTable: Map[String, List[(String, String, Int)]] = Map()

    Source.fromInputStream(in).getLines().foreach {
      case ConceptLineRegexp(phrase, concept, countStr) =>
        val firstWordInPhrase = phrase.split(" ").head
        conceptTable(firstWordInPhrase) = (phrase, concept, countStr.toInt) :: conceptTable.getOrElse(firstWordInPhrase, List())
        phraseCounts(phrase) = 0
      case line: String =>
        throw new RuntimeException("WTF")
    }

    err.println("Counting phrases in corpus: " + inputTokenizeSentences)

    // Внутри - самый топорный алгоритм поска подстроки...
    for (sentence <- fromFile(inputTokenizeSentences).getLines) {
      val words = sentence.split(" ")
      for ((word, wordIdx) <- words.zipWithIndex) {
        val matchingConcepts = conceptTable.getOrElse(word, List()).filter {
          case (phrase, _, _) =>
            val phraseLength = phrase.split(" ").length
            val possiblePhrase = words.slice(wordIdx, wordIdx + phraseLength).mkString(" ")
            phrase == possiblePhrase
        }

        for ((phrase, _, _) <- matchingConcepts) {
          phraseCounts(phrase) = phraseCounts(phrase) + 1
        }
      }
    }

    err.println("Writing out concept table")

    for {(firstWord, list) <- conceptTable
         (phrase, concept, count) <- list} {
      out.println(s"$phrase ||| " +
                    s"$concept ||| " +
                    s"N=${count.toString} " +
                    s"c|p=${Math.log(count.toDouble / phraseCounts(phrase).toDouble).toString}")
    }

  }
}