package edu.cmu.lti.nlp.amr.FastFeatureVector
import edu.cmu.lti.nlp.amr._
import edu.cmu.lti.nlp.amr.graph.Graph

case class DecoderResult(graph: Graph, features: FeatureVector, score: Double)

