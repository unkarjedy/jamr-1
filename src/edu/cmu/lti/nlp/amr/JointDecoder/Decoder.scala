package edu.cmu.lti.nlp.amr.JointDecoder
import edu.cmu.lti.nlp.amr._
import edu.cmu.lti.nlp.amr.FastFeatureVector._

import scala.collection.mutable.Map
import scala.collection.mutable.Set
import scala.collection.mutable.ArrayBuffer

abstract class Decoder {
    val stage1Features : ConceptInvoke.ConceptFeatures
    val stage2Features : GraphDecoder.Features
    val weights : FeatureVectorFast

    def decode(i: Input) : DecoderResult
}

