package edu.cmu.lti.nlp.amr.FastFeatureVector

import java.nio.file.{Files, Paths}

import edu.cmu.lti.nlp.amr.Train._
import edu.cmu.lti.nlp.amr._
import resource.managed

import scala.collection.mutable.Map
import scala.io.Source.fromFile
import scala.math.sqrt
import scala.pickling.Defaults._
import scala.pickling.json._
import scala.util.Random

class AdagradFast extends Optimizer[FeatureVectorFast] {
  def learnParameters(gradient: (Option[Int], Int, FeatureVectorFast) => (FeatureVectorFast, Double),
                      initialWeights: FeatureVectorFast,
                      trainingSize: Int,
                      noreg: List[String],
                      trainingObserver: (Int, FeatureVectorFast) => Boolean,
                      options: Map[Symbol, String]): FeatureVectorFast = {
    val passes = options('trainingPasses).toInt
    val stepsize = options('trainingStepsize).toDouble
    val l2strength = options('trainingL2RegularizerStrength).toDouble
    val avg = options.contains('trainingAvgWeights)
    val warmStartInterval = options('trainingWarmStartSaveInterval).toInt
    val warmStartFilename: Option[String] = options.get('trainingWarmStartSaveFile)

    val weights = FeatureVectorFast(initialWeights.labelset)
    weights += initialWeights
    val avg_weights = FeatureVectorFast(weights.labelset)
    val sumSq = FeatureVectorFast(weights.labelset) // G_{i,i}
    var pass = 0

    ////////////// Reload for warm start ////////////
    case class WarmStart(pass: Int, t: Int, trainSequence: Array[Int], weights: String, avg_weights: String)
    var warmStart: Option[WarmStart] = None
    if (warmStartFilename.isDefined && Files.exists(Paths.get(warmStartFilename.get))) {
      val lines: String = managed(fromFile(warmStartFilename.get)).acquireAndGet(_.getLines().mkString("\n"))
      warmStart = Some(lines.unpickle[WarmStart]) // see http://stackoverflow.com/questions/23072118/scala-pickling-how
      weights.read(warmStart.get.weights.split("\n").iterator)
      avg_weights.read(warmStart.get.avg_weights.split("\n").iterator)
    }

    while (pass < passes && (pass == 0 || trainingObserver(pass, avg_weights))) {
      logger(-1, "Pass " + (pass + 1).toString)
      var objective = 0.0 // objective is 1/N \sum_i=1^N Loss(i) + 1/2 * \lambda * ||weights||^2 (var objective is N times this)
      var trainSequence: Array[Int] = Random.shuffle(Range(0, trainingSize).toList).toArray
      if (warmStart.isDefined) {
        trainSequence = warmStart.get.trainSequence
        warmStart = None
      }

      for (t <- trainSequence) {
        // normally we would do weights -= stepsize * gradient(t)._1
        // but instead we do this: (see equation 8 in SocherBauerManningNg_ACL2013.pdf)
        val (grad, score) = gradient(Some(pass), t, weights)
        //logger(1, "--- Gradient ---")
        //logger(1, grad)
        sumSq.update(grad, (feat, label, x, y) => x + y * y)
        weights.update(grad, (feat, label, x, y) => {
          val sq = sumSq(feat, label)
          if (sq > 0.0) {
            x - stepsize * y / sqrt(sumSq(feat, label))
          } else {
            x
          }
        })
        objective += score
        if (l2strength != 0.0) {
          val noregSaveValues = noreg.map(feat => (feat, weights.fmap(feat)))
          noreg.map(feat => weights.fmap.remove(feat))
          objective += weights.dot(weights) / 2.0 // don't count the unregularized features in the regularizer
          sumSq.update(weights, (feat, label, x, y) => x + l2strength * l2strength * y * y)
          weights.update(weights, (feat, label, x, y) => {
            val sq = sumSq(feat, label)
            if (sq > 0.0) {
              x - stepsize * l2strength * y / sqrt(sumSq(feat, label))
            } else {
              x
            }
          })
          noregSaveValues.foreach(x => {
            weights.fmap(x._1) = x._2
          })
        }

        ///////////// Save for warm start /////////////
        if (warmStartFilename.isDefined && t % warmStartInterval == warmStartInterval - 1) {
          val save = WarmStart(pass, t, trainSequence, weights.toString(), avg_weights.toString())
          val str: String = save.pickle.value // see http://stackoverflow.com/questions/23072118/scala-pickling-how
          for (outputFile <- managed(new java.io.PrintWriter(new java.io.File(warmStartFilename.get), "UTF-8"))) { // see http://jsuereth.com/scala-arm/usage.html
            outputFile.println(str)
          }
        }
      }
      logger(-1, "                                   Avg objective value last pass: " + (objective / trainingSize.toDouble).toString)
      //logger(0,"                                                       objective: "+((0 until trainingSize).map(x => gradient(None, x, weights)._2).sum/trainingSize).toString)
      avg_weights += weights
      pass += 1
    }
    trainingObserver(pass, avg_weights)
    /////////// Remove warmStart file //////////
    if (warmStartFilename.isDefined) {
      Files.delete(Paths.get(warmStartFilename.get))
    }

    if (avg) {
      avg_weights
    } else {
      weights
    }
  }
}

