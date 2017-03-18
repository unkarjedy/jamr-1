package edu.cmu.lti.nlp.amr.ConceptInvoke

import edu.cmu.lti.nlp.amr.BasicFeatureVector._
import edu.cmu.lti.nlp.amr._

abstract class Decoder(featureNames: List[String],
                       phraseCounts: Map[List[String], Int]) {
  val features = new Features(featureNames, phraseCounts) // maybe this should be renamed ff?

  def decode(input: Input,
             trainingIndex: Option[Int],
             cost: (Input, PhraseConceptPair, Int, Int, List[PhraseConceptPair]) => Double = (i, c, s, p, l) => 0
            ): DecoderResult
}

