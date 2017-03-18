package edu.cmu.lti.nlp.amr.utils

case class F1(var correct: Double, var predicted: Double, var total: Double) {
  def precision: Double = correct / predicted
  def recall: Double = correct / total
  def f1: Double = 2 * (precision * recall) / (precision + recall)

  override def toString: String = {
    s"""Precision: ${precision.toString}
       |Recall: ${recall.toString}
       |F1: ${f1.toString}""".stripMargin
  }
}