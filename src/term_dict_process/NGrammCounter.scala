package term_dict_process

import edu.stanford.nlp.util.StringUtils

import scala.collection.JavaConverters._
import scala.collection.mutable

case class BiGram(first: String, second: String)

class NGrammCounter(allSentences: mutable.Seq[String]) {

  def countBigrams(): Seq[(String, Int)] = {
    val (minSize, maxSize) = (2, 2)
    allSentences
      .flatMap { sent => StringUtils.getNgramsString(sent, minSize, maxSize).asScala }
      .groupBy(identity)
      .mapValues(_.size)
      .toSeq
  }

}
