package term_dict_process

import scala.collection.mutable

/**
  * Actually should be a normal parser with some grammar... but no time...
  */
class SenteseSplitter(terms: Seq[Term]) {

  private val termsStartingWithDotLowercased: Seq[String] = {
    terms.map(_.value)
      .filter(_.startsWith("."))
      .map(s => s.indexOf(' ') match {
        case -1 => s
        case id => s.substring(0, id)
      })
  }

  /** Just do not try to read the method.
    * Lots of heuristics used based on raw definitions text. */
  def split(text: String): Seq[String] = {
    val sentences = mutable.ArrayBuffer[String]()
    val sentenceBuilder = StringBuilder.newBuilder

    def finishCurrentSentence() = {
      sentences += sentenceBuilder.toString()
      sentenceBuilder.clear()
    }

    var isOpenQuote = false
    var roundBracketDeepness = 0 // (
    var ch: Char = ' '
    text.indices.foreach(index => {
      ch = text.charAt(index)

      val _isBeginingOfTerm = isBeginningOfTermWithDot(index, text)
      val _isDotInsideSomeLexem = isDotInsideSomeLexem(index, text)

      val isEndOfSentence = {
        ch == '.' &&
          roundBracketDeepness <= 0 &&
          !isOpenQuote &&
          !_isBeginingOfTerm &&
          !_isDotInsideSomeLexem &&
          isNextCharUpperCase(index, text)
      }

      val skipAppending = ch == '.' && !_isBeginingOfTerm && !_isDotInsideSomeLexem

      if (isEndOfSentence) {
        finishCurrentSentence()
      } else if (!skipAppending) {
        sentenceBuilder.append(ch)
      }

      ch match {
        case '(' => roundBracketDeepness += 1
        case ')' => roundBracketDeepness -= 1
        case '"' => isOpenQuote = !isOpenQuote
        case _ =>
      }

      roundBracketDeepness = Math.min(0, roundBracketDeepness) // for broken brackets: "()))) (()) ()()"
    })

    finishCurrentSentence()

    sentences.map(_.trim).filter(_.nonEmpty)
  }

  private def isBeginningOfTermWithDot(index: Int, text: String): Boolean = {
    termsStartingWithDotLowercased.exists(term => {
      val end = Math.min(index + term.length, text.length)
      term.compareToIgnoreCase(text.substring(index, end)) == 0
    })
  }

  private def isDotInsideSomeLexem(index: Int, text: String): Boolean = {
    val left = index - 1
    val right = index + 1

    left >= 0 && right < text.length &&
      !text.charAt(left).isWhitespace &&
      !text.charAt(right).isWhitespace
  }

  def isNextCharUpperCase(index: Int, text: String): Boolean = {
    var id = index + 1
    while (id < text.length && (
      text.charAt(id).isWhitespace || text.charAt(id) == '.')
    ) {
      id += 1
    }
    id < text.length && text.charAt(id).isUpper
  }

}
