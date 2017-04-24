package edu.cmu.lti.nlp.amr.Train

import scala.collection.mutable.Map

abstract class Optimizer[FeatureVector <: FeatureVectorAbstract] {
    def learnParameters(gradient: (Int, FeatureVector) => (FeatureVector, Double),
                        initialWeights: FeatureVector,
                        trainingSize: Int,
                        noreg: List[String],
                        options: Map[Symbol, String]) : FeatureVector = {
        val myGrad : (Option[Int], Int, FeatureVector) => (FeatureVector, Double) = (pass, i, w) => gradient(i,w)
        learnParameters(myGrad, initialWeights, trainingSize, noreg, (x: Int, w: FeatureVector) => true, options)
    }

    def learnParameters(gradient: (Int, FeatureVector) => (FeatureVector, Double),
                        initialWeights: FeatureVector,
                        trainingSize: Int,
                        noreg: List[String],
                        trainingObserver: (Int, FeatureVector) => Boolean,
                        options: Map[Symbol, String]) : FeatureVector = {
        val myGrad : (Option[Int], Int, FeatureVector) => (FeatureVector, Double) = (pass, i, w) => gradient(i,w)
        learnParameters(myGrad, initialWeights, trainingSize, noreg, trainingObserver, options)
    }

    def learnParameters(gradient: (Option[Int], Int, FeatureVector) => (FeatureVector, Double),  // Input: (pass, i, weights) Output: (gradient, objective value)
                        initialWeights: FeatureVector,
                        trainingSize: Int,
                        noreg: List[String],    // features not regularized
                        trainingObserver: (Int, FeatureVector) => Boolean,  // Input: pass, weights  Output: true stops training loop
                        options: Map[Symbol, String]) : FeatureVector

}

