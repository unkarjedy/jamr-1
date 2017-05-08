package term_dict_process

import edu.stanford.nlp.util.StringUtils

import scala.collection.mutable
import scala.collection.JavaConverters._

case class BiGram(first: String, second: String)

class NGrammCounter(allSentences: mutable.Seq[String]) {

  def countBigrams(): Seq[(String, Int)] = {
    allSentences.flatMap(sent => StringUtils.getNgramsString(sent, 2, 2).asScala)
      .groupBy(identity)
      .mapValues(_.size)
      .toSeq
  }

}
