package edu.cmu.lti.nlp.amr
import scala.language.implicitConversions

import scala.collection.concurrent.{TrieMap => Map}

package object FastFeatureVector {
    implicit def doubleToFastMulAssoc(x: Double) = new FastMulAssoc(x)
    implicit def doubleToFastMul2Assoc(x: Double) = new FastMul2Assoc(x)
    def fromBasicFeatureVector(basicFeatureVector: BasicFeatureVector.FeatureVectorBasic, labelset: Array[String]) : FeatureVectorFast = {
        return FeatureVectorFast(labelset, Map.empty[String, ValuesMap] ++= basicFeatureVector.fmap.map(x => (x._1, ValuesMap(x._2, Map.empty[Int,Double]))))
    }
}

