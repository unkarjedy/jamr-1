package edu.cmu.lti.nlp.amr.ConceptInvoke

import edu.cmu.lti.nlp.amr.BasicFeatureVector._
import edu.cmu.lti.nlp.amr._

abstract class ConceptDecoderAbstract(featureNames: List[String],
                                      phraseCounts: Map[List[String], Int]) {
  val features = new ConceptFeatures(featureNames, phraseCounts) // maybe this should be renamed ff?

  type ExtraCostFunc = (Input, PhraseConceptPair, Int, Int, List[PhraseConceptPair]) => Double
  private val zeroExtraCost: ExtraCostFunc = (i, c, s, p, l) => 0

  def decode(input: Input,
             trainingIndex: Option[Int],
             extraCost: ExtraCostFunc = zeroExtraCost): DecoderResult
}

