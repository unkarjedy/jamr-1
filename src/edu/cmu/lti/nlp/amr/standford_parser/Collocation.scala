package edu.cmu.lti.nlp.amr.standford_parser

class Collocation(phrase: String) {
  var POS: String = "NN"
  def str: String = phrase
  def strGlued = str.replaceAll("\\s+", "")
}

object Collocation {
  def apply(phrase: String): Collocation = new Collocation(phrase)
}

class CollocationDictionary