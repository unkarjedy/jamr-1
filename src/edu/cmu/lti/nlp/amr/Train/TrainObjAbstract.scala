package edu.cmu.lti.nlp.amr.Train

import java.io.{File, FileWriter, PrintStream}
import java.util.concurrent.TimeUnit

import edu.cmu.lti.nlp.amr.BasicFeatureVector.{AdagradBasic, FeatureVectorBasic, SSGDBasic}
import edu.cmu.lti.nlp.amr.FastFeatureVector.{AdagradFast, FeatureVectorFast, SSGDFast}
import edu.cmu.lti.nlp.amr._

import scala.collection.mutable.Map

abstract class TrainObjAbstract[FeatureVector <: FeatureVectorAbstract](
                                                                         featureVectorClass: Class[_ >: FeatureVector],
                                                                         options: Map[Symbol, String]
                                                                       ) {
  if (options.contains('trainingSaveInterval) && !options.contains('trainingOutputFile)) {
    System.err.println("Error: trainingSaveInterval specified but output weights filename given")
    sys.exit(1)
  }

  ////////////////// Default Options ////////////////
  options('trainingPasses) = options.getOrElse('trainingPasses, "20")
  options('trainingStepsize) = options.getOrElse('trainingStepsize, "1.0")
  options('trainingL2RegularizerStrength) = options.getOrElse('trainingL2RegularizerStrength, "0.0")
  options('trainingWarmStartSaveInterval) = options.getOrElse('trainingWarmStartSaveInterval, "200")

  ////////////////// Training Setup ////////////////
  private val loss = options.getOrElse('trainingLoss, "Perceptron")
  private val numThreads = options.getOrElse('numThreads, "4").toInt

  private var optimizer: Optimizer[FeatureVector] = {
    val method = options.getOrElse('trainingOptimizer, "Adagrad")

    val FVBasic = classOf[FeatureVectorBasic]
    val FVFast = classOf[FeatureVectorFast]
    val result = (method, featureVectorClass) match {
      case ("Adagrad", FVBasic) => new AdagradBasic()
      case ("Adagrad", FVFast) => new AdagradFast(logProgress)
      case ("SSGD", FVBasic) => new SSGDBasic()
      case ("SSGD", FVFast) => new SSGDFast()
      case x =>
        System.err.println("Error: unknown training optimizer " + x)
        sys.exit(1)
    }

    // no actual conversion proceeded for FeatureVector cause it is a generic type, it is needed just for compiler
    result.asInstanceOf[Optimizer[FeatureVector]]
  }

  private val progressFileOpt = options.get('trainingOutputFile).map(new File(_).toPath.getParent.resolve("progress.log").toFile)
  private val progressOutOpt = progressFileOpt.map(new PrintStream(_))
  private var prevPass = -1
  private var prevTimeMs = -1L
  private var timeTotalMs = 0L

  private def logProgress(pass: Int, elementsProcessed: Int, elementsTotal: Int, currentResult: FeatureVectorFast): Unit = {
    progressOutOpt.foreach(out => {
      if(pass != prevPass) {
        prevTimeMs = System.currentTimeMillis()
        out.println(pass)
        prevPass = pass
      }

      val passFinished = elementsProcessed == elementsTotal
      if (passFinished) {
        val deltaTimeMs = (System.currentTimeMillis() - prevTimeMs)
        timeTotalMs += deltaTimeMs
        out.println(s"Time spent for pass $pass: ${timeMsToString(deltaTimeMs)} (total: ${timeMsToString(timeTotalMs)})")
        out.println()
      }

      out.println(s"$elementsProcessed / $elementsTotal")
    })
  }

  private def timeMsToString(deltaTimeMs: Long) = {
    val deltaMin = TimeUnit.MILLISECONDS.toMinutes(deltaTimeMs)
    val deltaSec = TimeUnit.MILLISECONDS.toSeconds(deltaTimeMs)
    val secRamain = deltaSec - TimeUnit.MINUTES.toSeconds(deltaMin)
    s"$deltaMin min, $secRamain sec"
  }

  if (options.getOrElse('trainingMiniBatchSize, "1").toInt > 1) {
    optimizer = new MiniBatch(optimizer, options('trainingMiniBatchSize).toInt, numThreads)
  }

  def decode(i: Int, weights: FeatureVector): (FeatureVector, Double, String)
  def oracle(i: Int, weights: FeatureVector): (FeatureVector, Double)
  def costAugmented(i: Int, weights: FeatureVector, scale: Double): (FeatureVector, Double)
  def train(): Unit
  def evalDev(options: Map[Symbol, String], pass: Int, weights: FeatureVector): Unit
  def zeroVector: FeatureVector
  def trainingSize: Int

  /////////////////////////////////////////////////
  def gradient(inputId: Int, weights: FeatureVector): (FeatureVector, Double) = {
    val scale = options.getOrElse('trainingCostScale, "1.0").toDouble
    try {
      if (loss == "Perceptron") {
        val (grad, score, _) = decode(inputId, weights)
        val (gradOracle, scoreOracle) = oracle(inputId, weights)
        grad -= gradOracle
        //logger(0, "Gradient:\n"+grad.toString)
        (grad, score - scoreOracle)
      } else if (loss == "SVM") {
        val (grad, score) = costAugmented(inputId, weights, scale)
        val (gradOracle, scoreOracle) = oracle(inputId, weights)
        grad -= gradOracle
        (grad, score - scoreOracle)
      } else if (loss == "Ramp1") {
        val (grad, score) = costAugmented(inputId, weights, scale)
        val o = decode(inputId, weights)
        grad -= o._1
        (grad, score - o._2)
      } else if (loss == "Ramp2") {
        val (grad, score, _) = decode(inputId, weights)
        val o = costAugmented(inputId, weights, -1.0 * scale)
        grad -= o._1
        (grad, score - o._2)
      } else if (loss == "Ramp3") {
        val (grad, score) = costAugmented(inputId, weights, scale)
        val o = costAugmented(inputId, weights, -1.0 * scale)
        grad -= o._1
        (grad, score - o._2)
      } else if (loss == "Infinite_Ramp" || loss == "Latent_Hinge") {
        // I called this Latent_Hinge earlier
        val (grad, score) = costAugmented(inputId, weights, scale)
        val (costFeats, costScore) = costAugmented(inputId, weights, -100000000000.0)
        grad -= costFeats
        (grad, score - costScore)
      } else {
        System.err.println("Error: unknown training loss " + loss)
        sys.exit(1)
      }
    } catch {
      case e: Throwable => if (options.contains('ignoreParserErrors)) {
        logger(-1, " ********** THERE WAS AN EXCEPTION IN THE PARSER. *********")
        if (verbosityGlobal >= -1) {
          e.printStackTrace()
        }
        logger(-1, "Continuing. To exit on errors, please run without --ignore-parser-errors")
        (zeroVector, 0.0)
      } else {
        throw e
      }
    }
  }

  def trainingObserver(pass: Int, weights: FeatureVector): Boolean = {
    val notFirstPass = pass > 0
    val logTrainingProcess = options.contains('trainingSaveInterval)
    val isLoggedPass = pass % options('trainingSaveInterval).toInt == 0

    if (logTrainingProcess && isLoggedPass && notFirstPass) {
      val iterationFileName = options('trainingOutputFile) + ".iter" + pass.toString
      val file = new java.io.PrintWriter(new java.io.File(iterationFileName), "UTF-8")
      try {
        file.print(weights.toString)
      }
      finally {
        file.close()
      }
    }
    evalDev(options, pass, weights)
    true
  }

  def train(initialWeights: FeatureVector) {
    val weights = optimizer.learnParameters(
      (i, w) => gradient(i, w),
      initialWeights,
      trainingSize,
      List("Bias", "bias"), // don't regularize the bias terms
      trainingObserver,
      options
    )

    System.err.print("Writing out weights... ")
    if (options.contains('trainingWeightsFile)) {
      val file = new java.io.PrintWriter(new java.io.File(options('trainingWeightsFile)), "UTF-8")
      try {
        file.print(weights.toString)
      }
      finally {
        file.close()
      }
    } else {
      System.out.print(weights.unsorted)
    }
    System.err.println("done")
  }
}

