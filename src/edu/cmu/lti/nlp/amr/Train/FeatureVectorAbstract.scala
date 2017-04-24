package edu.cmu.lti.nlp.amr.Train

abstract class FeatureVectorAbstract(labelset: Array[String]) {
  def +=(v: FeatureVectorAbstract): Unit
  def -=(v: FeatureVectorAbstract): Unit
  def unsorted: String
}

