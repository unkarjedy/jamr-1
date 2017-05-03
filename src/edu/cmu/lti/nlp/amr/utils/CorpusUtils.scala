package edu.cmu.lti.nlp.amr.utils

object CorpusUtils {
  // This treats more than one newline in a row as a single newline
  def splitOnNewline(iterator: Iterator[String]): Iterator[String] = {
    for {
      x <- iterator if x != ""
      p = (x :: iterator.takeWhile(_ != "").toList).mkString("\n")
    } yield p
  }

  /**
    * Takes an iterator of lines, splits on empty lines, and yields only
    * blocks of lines that contain some AMR content
    */
  def getAMRBlocks(iterator: Iterator[String]): Iterator[String] = for (
    block <- splitOnNewline(iterator)
    if block.split("\n").exists(_.startsWith("(")) // needs to contain some AMR
  ) yield block
}

class CorpusUtilsTest /* extends Suite*/ {
  def testSplitOnNewline() {
    val split = CorpusUtils.splitOnNewline(Iterator("a", "b", "c", "", "a", "c", "b"))
    assert(split.toList == List("a\nb\nc", "a\nc\nb"))
  }
}

