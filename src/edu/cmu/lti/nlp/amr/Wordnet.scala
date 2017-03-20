package edu.cmu.lti.nlp.amr

import scala.collection.JavaConversions._
import scala.collection.JavaConverters._

import java.io.File
import java.net.URL

import edu.mit.jwi.{IRAMDictionary, RAMDictionary}
import edu.mit.jwi.data.ILoadPolicy
import edu.mit.jwi.item.POS
import edu.mit.jwi.morph.WordnetStemmer

object Wordnet {
  private var wordnetStemmer = Option(System.getenv("WNHOME")).map(newWordNetStemmer).orNull

  def prepareWordNetStemmer(wnHome: String): Unit = {
    wordnetStemmer = newWordNetStemmer(wnHome)
  }

  private def newWordNetStemmer(wnHome: String): WordnetStemmer = {
    val path = wnHome + File.separator + "dict"
    val url = new URL("file", null, path)
    val dict = new RAMDictionary(url, ILoadPolicy.NO_LOAD)
    dict.open
    new WordnetStemmer(dict)
  }


  def stemmer(word: String): List[String] = {
    var stems = List[String]()
    for (pos <- POS.values) {
      try {
        stems ++= wordnetStemmer.findStems(word, pos)
      }
      catch {
        case e: Throwable => Unit
      }
    }

    stems.distinct.sorted
  }

  def stemmer(word: String, pos: POS): List[String] = {
    try {
      wordnetStemmer.findStems(word, pos).asScala.toList
    }
    catch {
      case e: Throwable => List()
    }
  }

}

