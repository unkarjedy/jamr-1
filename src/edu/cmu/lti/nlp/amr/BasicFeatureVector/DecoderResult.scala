package edu.cmu.lti.nlp.amr.BasicFeatureVector
import edu.cmu.lti.nlp.amr._
import edu.cmu.lti.nlp.amr.graph.Graph

case class DecoderResult(graph: Graph, features: FeatureVectorBasic, score: Double)

