package edu.cmu.lti.nlp.amr

import scala.collection.JavaConversions._
import scala.collection.JavaConverters._
import java.io.File
import java.net.URL

import edu.mit.jwi.{IRAMDictionary, RAMDictionary}
import edu.mit.jwi.data.ILoadPolicy
import edu.mit.jwi.item.POS
import edu.mit.jwi.morph.WordnetStemmer

import scala.collection.mutable

object Wordnet {
  private var wordnetStemmer = Option(System.getenv("WNHOME")).map(newWordNetStemmer).orNull

  def prepareWordNetStemmer(wordnetHome: String): Unit = {
    wordnetStemmer = newWordNetStemmer(wordnetHome)
  }

  private def newWordNetStemmer(wordnetHomePath: String): WordnetStemmer = {
    val path = wordnetHomePath + File.separator + "dict"
    val url = new URL("file", null, path)
    val dict = new RAMDictionary(url, ILoadPolicy.NO_LOAD)
    dict.open
    new WordnetStemmer(dict)
  }


  private val wordCache = mutable.Map[String, List[String]]()
  private val wordPosCache = mutable.Map[(String, POS), List[String]]()

  def getStemms(word: String): List[String] = {
    val cached = wordCache.get(word)

    cached.getOrElse({
      var stems = POS.values.toList.flatMap(pos => getStemms(word, pos))
      stems = stems.distinct.sorted
      wordCache(word) = stems
      stems
    })
  }

  def getStemms(word: String, pos: POS): List[String] = {
    val cached = wordPosCache.get((word, pos))

    cached.getOrElse({
      try {
        val stems = wordnetStemmer.findStems(word, pos).asScala.toList
        wordPosCache((word, pos)) = stems
        stems
      }
      catch {
        case e: Throwable => List()
      }
    })
  }

}

