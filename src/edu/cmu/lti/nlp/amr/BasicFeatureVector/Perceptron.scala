package edu.cmu.lti.nlp.amr.BasicFeatureVector
import edu.cmu.lti.nlp.amr._
import edu.cmu.lti.nlp.amr.Train._

/******************************** Perceptron Training **********************************/

object Perceptron {
    def learnParameters(decoder: Int => FeatureVectorBasic,
                        oracle: Int => FeatureVectorBasic,
                        weights: FeatureVectorBasic,
                        trainingSize: Int,
                        passes: Int,
                        avg: Boolean) : FeatureVectorBasic = {
        var avg_weights = FeatureVectorBasic()
        val permutations = Range(0, trainingSize).permutations
        var corpus = permutations.next
        for (i <- Range(1,passes+1)) {
            logger(0,"Pass "+i.toString)
            if (permutations.hasNext) {
                corpus = permutations.next
            }
            for (t <- corpus) {
//                logger(1,"-- Weights --")
//                logger(1,weights)
                val minus = decoder(t)
                val plus = oracle(t)
                logger(1,"-- Good --")
                logger(1,plus)
                logger(1,"-- Bad --")
                logger(1,minus)
                weights -= minus
                weights += plus
                plus -= minus
                logger(1,"-- Difference --")
                logger(1,plus)
            }
            avg_weights += weights
        }
        if(avg) { avg_weights } else { weights }
    }
}

