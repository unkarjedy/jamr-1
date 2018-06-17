package edu.cmu.lti.nlp.amr

import scala.collection.mutable.Map

package object GraphDecoder {
  type OptionMap = Map[Symbol, String]

  def getFeatures(options: OptionMap): List[String] = {
    options.getOrElse('stage2Features, "conceptBigram,rootConcept").split(",").toList.filter(x => x != "edgeId" && x != "labelWithId")
  }

  def loadLabelset(filename: String): Array[(String, Int)] = {
    Source.fromFile(filename).getLines().toArray.map(x => {
      val split = x.split(" +")
      // NAUMENKO: I moved peace of code to variable `weight`, but actually i do not know what its meaning is
      val weight = if (split.size > 1) {
        split(1).toInt
      } else {
        1000
      }
      (split(0), weight)
    })
  }

  def getLabelset(options: OptionMap): Array[(String, Int)] = {
    return loadLabelset(options('stage2Labelset)) // TODO: check for errors
  }

  def buildGraphDecoder(options: OptionMap): GraphDecoder.GraphDecoderAbstract = {
    if (!options.contains('stage2Labelset)) {
      System.err.println("Error: No labelset file specified");
      sys.exit(1)
    }

    val labelset: Array[(String, Int)] = getLabelset(options)

    val features = getFeatures(options)
    logger(0, "Stage2 features = " + features)

    val connected = !options.contains('stage2NotConnected)
    logger(0, "connected = " + connected)

    if (!options.contains('stage2Decoder)) {
      System.err.println("Error: No stage2 decoder specified");
      sys.exit(1)
    }

    val decoder: GraphDecoderAbstract = options('stage2Decoder) match {
      case "Alg1" => new Alg1(options, features, labelset)
      case "Alg1a" => new Alg1(options, features, labelset, connectedConstraint = "and")
      case "Alg2" => new Alg2(options, features, labelset, connected)
      case "Greedy" => new Greedy(options, features, labelset)
      //case "DD" => new DualDecomposition(features, labelset, 1)
      case "LR" => new LagrangianRelaxation(options, features, labelset)
      case x =>
        System.err.println("Error: unknown stage2 decoder " + x);
        sys.exit(1)
    }

    val outputFormat = options.getOrElse('outputFormat, "triples").split(",").toList
    if (outputFormat.contains("AMR") && !connected) {
      System.out.println("Cannot have both -stage2NotConnected flag and --outputFormat \"AMR\"");
      sys.exit(1)
    }

    if (options('stage2Decoder) == "Alg1" && outputFormat.contains("AMR")) {
      System.err.println("Cannot have --outputFormat \"AMR\" for stage2 Alg1 (graph may not be connected!)")
      sys.exit(1)
    }

    return decoder
  }

  def buildGraphDecoderOracle(options: OptionMap): GraphDecoder.GraphDecoderAbstract = {
    new Oracle(options, getFeatures(options), getLabelset(options).map(x => x._1))
  }

  def buidGraphDecoderCostAugmented(options: OptionMap): GraphDecoder.GraphDecoderAbstract = {
    val decoder = buildGraphDecoder(options)
    new CostAugmented(decoder, -100000000000.0, 0.5, options)
  }
}

