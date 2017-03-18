package edu.cmu.lti.nlp.amr.utils

object LineUtils {

  def containsAmrLine(block: String): Boolean = {
    block.split("\n").exists(_.startsWith("("))
  }

  def cleanDependencyStr(str: String): String = {
    str.replaceAllLiterally("-LRB-", "(")
      .replaceAllLiterally("-RRB-", ")")
      .replaceAllLiterally("""\/""", "/")
  }

}
