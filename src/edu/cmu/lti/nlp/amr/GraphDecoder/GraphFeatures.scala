package edu.cmu.lti.nlp.amr.GraphDecoder

import edu.cmu.lti.nlp.amr.FastFeatureVector._
import edu.cmu.lti.nlp.amr._
import edu.cmu.lti.nlp.amr.graph.{Graph, Node}
import edu.cmu.lti.nlp.amr.utils.CorpusUtils

import scala.collection.mutable.Map
import scala.collection.{immutable, mutable}
import scala.io.Source.fromFile

/** ************************** Feature Functions *****************************/
// This object is a hack, should change so all inputs are sent in an XML container
object GraphFeatures {
  private var firstTime: Boolean = true
  private var srlArray: Array[String] = _

  def setUpInput(options: mutable.Map[Symbol, String], featureNames: List[String]) {
    if (firstTime) {
      firstTime = false
      if (featureNames.contains("srl")) {
        srlArray = CorpusUtils.splitOnNewline(fromFile(options('srl)).getLines).toArray
      }
    }
  }
}

class GraphFeatures(options: mutable.Map[Symbol, String],
                    private var myFeatureNames: List[String],
                    labelSet: Array[String]) {
  var weights = FeatureVectorFast(labelSet: Array[String])

  // TODO: maybe weights should be passed in to the constructor
  private var inputSave: Input = _
  private var graph: Graph = _
  private var sentence: Array[String] = _
  private var dependencies: Annotation[Dependency] = _
  private var fullPos: Annotation[String] = _

  // node1 is always the tail, and node2 the head
  private var node1: Node = _
  private var node2: Node = _

  private var srl: SRL = _

  /** (featureName, Value(unconjoined, conjoined), conjoinedMap)
    * do not know where conjoinedMap is used*/
  private var feats: List[(String, Value, immutable.Map[Int, Double])] = _

  private def node1SpanStart = graph.spans(node1.spans(0)).start
  private def node1SpanEnd = graph.spans(node1.spans(0)).end
  private def node2SpanStart = graph.spans(node2.spans(0)).start
  private def node2SpanEnd = graph.spans(node2.spans(0)).end

  def input: Input = inputSave
  def input_=(inputNew: Input) {
    GraphFeatures.setUpInput(options, featureNames)
    inputSave = inputNew
    graph = inputNew.graph.get
    sentence = inputNew.sentence
    dependencies = inputNew.dependencies
    fullPos = inputNew.pos
    if (featureNames.contains("srl")) {
      srl = SRL.fromString(GraphFeatures.srlArray(inputNew.index), dependencies)
    }

    precompute()
  }

  def featureNames: List[String] = myFeatureNames
  def featureNames_=(featNames: List[String]) {
    myFeatureNames = featNames
    featureFunctions = myFeatureNames.filter(x => !rootFFTable.contains(x)).map(x => ffTable(x)) // TODO: error checking on lookup
    rootFeatureFunctions = myFeatureNames.filter(x => rootFFTable.contains(x)).map(x => rootFFTable(x))
  }

  def addFeatureFunction(featureName: String) {
    if (!featureNames.contains(featureName)) {
      featureNames = featureName :: myFeatureNames // featureNames is a parametric field, and it updates featureFunctions
    }
  }

  /** Calculate the local features */
  def localFeatures(n1: Node, n2: Node): List[(String, Value, immutable.Map[Int, Double])] = {
    assert(featureNames.size == featureFunctions.size + rootFeatureFunctions.size, "featureNames.size != featureFunctions.size.  You must make sure to use setFeatures() or addFeatureFunction() to set the features if you change them after creating the decoder object.")
    node1 = n1
    node2 = n2
    feats = List()
    for (ff <- featureFunctions) {
      ff()
    }
    //logger(1, "localFeatures("+n1.id.toString+","+n2.id.toString+") = \n"+feats.sortBy(_._1).mkString("\n"))
    feats
  }

  def localFeatures(node1: Node, node2: Node, label: Int): List[(String, ValuesList)] = {
    localFeatures(node1, node2).map(
      x => (x._1, ValuesList(x._2.unconjoined + x._3.getOrElse(label, 0.0), List(Conjoined(label, x._2.conjoined))))
    )
  }

  def localFeatures(node1: Node, node2: Node, label: String): List[(String, ValuesList)] = {
    if (weights.labelToIndex.contains(label)) {
      localFeatures(node1, node2, weights.labelToIndex(label))
    } else {
      logger(0, "************* WARNING: Cannot find label = " + label + " in the labelset ***************")
      localFeatures(node1, node2).map(x => (x._1, ValuesList(x._2.unconjoined, List())))
    }
  }

  def localScore(node1: Node, node2: Node, label: String): Double = {
    weights.dot(localFeatures(node1, node2, label))
  }

  /** Calculate the root features */
  def rootFeatures(node: Node): List[(String, ValuesList)] = {
    node1 = node
    feats = List()
    for (ff <- rootFeatureFunctions) {
      ff()
    }
    feats.map(x => (x._1, ValuesList(x._2.unconjoined, List())))
  }

  def rootScore(node: Node): Double = {
    weights.dot(rootFeatures(node))
  }

  type FeatureFunction = () => Unit // function with no arguments and no return value

  private val ffTable = mutable.Map[String, FeatureFunction](
    "CostAugEdge" -> ffCostAugEdge _, // trailing _ for partially applied function
    "DDEdgeId" -> ffDDEdgeId _,
    "LRLabelWithId" -> ffLRLabelWithId _,
    "bias" -> ffBias _,
    "biasScaled" -> ffBiasScaled _,
    "biasCSuf" -> ffBiasCSuf _,
    "typeBias" -> ffTypeBias _,
    "self" -> ffSelf _,
    "fragHead" -> ffFragHead _,
    "edgeCount" -> ffEdgeCount _,
    "distance" -> ffDistance _,
    "logDistance" -> fflogDistance _,
    "conceptBigram" -> ffConceptBigram _,
    "conceptUnigramWithLabel" -> ffConceptUnigramWithLabel _,
    "posPathv1" -> ffPosPathUnigramBigramv1 _,
    "posPathv2" -> ffPosPathUnigramBigramv2 _,
    "posPathv3" -> ffPosPathUnigramBigramv3 _,
    //"dependencyPathv1" -> ffDependencyPathv1 _,
    "dependencyPathv2" -> ffDependencyPathv2 _,
    "dependencyPathv3" -> ffDependencyPathv3 _,
    "dependencyPathv4" -> ffDependencyPathv4 _,
    "dependencyPathv5" -> ffDependencyPathv5 _,
    "srl" -> ffSRL _
  )

  val rootFFTable = Map[String, FeatureFunction](
    "rootConcept" -> ffRootConcept _,
    "rootCostAug" -> ffRootCostAug _,
    "rootDependencyPathv1" -> ffRootDependencyPathv1 _
  )

  var featureFunctions: List[FeatureFunction] = {
    featureNames.filter(name => !rootFFTable.contains(name)).map(x => {
      if (ffTable.contains(x)) {
        ffTable(x)
      } else {
        System.err.println("Error: Unknown feature " + x)
        sys.exit(1)
      }
    })
  }

  var rootFeatureFunctions: List[FeatureFunction] = {
    featureNames.filter(x => rootFFTable.contains(x)).map(x => rootFFTable(x))
  }

  /** For each dependencies.tok holds a dependecy path to the dependency tree ROOT */
  var rootDependencyPaths: Array[List[Int]] = _

  def precompute(): Unit = {
    rootDependencyPaths = dependencies.tok.indices.map(tokId => findRootDependencyPath(tokId)).toArray
    logger(1, "rootDependencyPaths = " + rootDependencyPaths.toList)
  }

  def addFeature(featureName: String, unconjoined: Double, conjoined: Double): Unit = {
    feats = (featureName, Value(unconjoined, conjoined), immutable.Map.empty[Int, Double]) :: feats
  }

  def addFeature(featureName: String, unconjoined: Double, conjoined: Double, conjoinedMap: immutable.Map[Int, Double]): Unit = {
    feats = (featureName, Value(unconjoined, conjoined), conjoinedMap) :: feats
  }

  def ffCostAugEdge(n1: Node, n2: Node): List[(String, Value, immutable.Map[Int, Double])] = {
    node1 = n1
    node2 = n2
    feats = List()
    ffCostAugEdge
    feats
  }

  /** Used for cost augmented decoding */
  def ffCostAugEdge: Unit = {
    if (node1.spans(0) != node2.spans(0)) {
      // WARNING: don't change this without also changing the features in CostAugmented decoder as well
      addFeature("CA:U_C1=" + node1.concept + "+C2=" + node2.concept, 1.0, 0.0)
      addFeature("CA:C1=" + node1.concept + "+C2=" + node2.concept, 0.0, 1.0)
    }
  }

  /** Used for Dual Decomposition */
  def ffDDEdgeId {
    addFeature("DD:Id1=" + node1.id, 0.0, 1.0)
    //return FeatureVector(Map(("DD:Id1="+node1.id+"+Id2="+node2.id+"+L="+label) -> 1.0))
  }

  def ffLRLabelWithId(n1: Node, n2: Node): List[(String, Value, immutable.Map[Int, Double])] = {
    node1 = n1
    node2 = n2
    feats = List()
    ffLRLabelWithId
    feats
  }

  def ffLRLabelWithId {
    // Used for Langragian Relaxation
    addFeature("LR:Id1=" + node1.id, 0.0, 1.0)
    //return FeatureVector(Map(("LR:Id1="+node1.id+"+L="+label) -> 1.0))
  }

  /** Bias features are unregularized.
    * Adjusting these values only adjusts the condition number of the optimization problem. */
  def ffBias: Unit = {
    // TODO: after testing, adjust back to 0.01
    addFeature("Bias", 0.0, 1.0)
    //return FeatureVector(Map(("L="+label) -> 1.0))
  }

  /** Bias features are unregularized.
    * Adjusting these values only adjusts the condition number of the optimization problem. */
  def ffBiasScaled: Unit = {
    addFeature("Bias.01", 0.01, 0.01)
  }

  // TODO: remove (legacy feature for reproducable results, same as ffBias)
  def ffEdgeCount: Unit = {
    addFeature("Edge", 1.0, 0.0)
  }

  def ffBiasCSuf {
    val c1size = node1.concept.size
    val c2size = node2.concept.size
    addFeature("C1Suf3=" + node1.concept.slice(c1size - 3, c1size), 1.0, 1.0)
    addFeature("C2Suf3=" + node2.concept.slice(c2size - 3, c2size), 1.0, 1.0)
  }

  // TODO
  def ffTypeBias: Unit = {
    def conceptType(x: String): String = if (x.matches(".*-[0-9][0-9]")) "E" /*Event*/ else "O"
    def labelType(x: String): String = if (x.startsWith(":ARG")) "A" else "O"

    val conceptType1 = conceptType(node1.concept)
    val conceptType2 = conceptType(node2.concept)

    addFeature("C1T=" + conceptType1, 1.0, 1.0)
    addFeature("C2T=" + conceptType2, 1.0, 1.0)

    // label index -> 1.0
    val argMap = weights.labelToIndex.toMap.filter(x => x._1.startsWith(":ARG")).map(x => (x._2, 1.0))
    val nonArgMap = weights.labelToIndex.toMap.filter(x => !x._1.startsWith(":ARG")).map(x => (x._2, 1.0))
    addFeature("C1T=" + conceptType1 + "LT=A", 0.0, 0.0, argMap)
    addFeature("C2T=" + conceptType2 + "LT=A", 0.0, 0.0, argMap)
    addFeature("C1T=" + conceptType1 + "LT=O", 0.0, 0.0, nonArgMap)
    addFeature("C2T=" + conceptType2 + "LT=O", 0.0, 0.0, nonArgMap)
  }


  def ffSelf: Unit = {
    val nodesInSameFragment = node1.spans(0) == node2.spans(0)
    val unconjoinedValue = if (nodesInSameFragment) 1.0 else 0.0
    addFeature("Slf", unconjoinedValue, 0.0)
  }

  // TODO: I'm assuming it is unlikely there are two identical concepts in a frag
  def ffFragHead {
    def isNodeFragmentHead(node: Node) = {
      node.concept == graph.spans(node.spans(0)).amrNode.concept
    }

    addFeature("C1NotFragHead", if (!isNodeFragmentHead(node1)) 1.0 else 0.0, 0.0)
    addFeature("C2NotFragHead", if (!isNodeFragmentHead(node2)) 1.0 else 0.0, 0.0)
  }

  private def calcDistance(nodeA: Node, nodeB: Node): Int = {
    val dist1 = Math.abs(graph.spans(nodeA.spans(0)).start - graph.spans(nodeB.spans(0)).end)
    val dist2 = Math.abs(graph.spans(nodeA.spans(0)).end - graph.spans(nodeB.spans(0)).start)
    min(dist1, dist2)
  }

  private def calcDirection(nodeA: Node, nodeB: Node, extra: Float = 0.0f): Float = {
    signum(graph.spans(nodeA.spans(0)).start - graph.spans(nodeB.spans(0)).end + extra)
  }

  def ffDistancev0: Unit = {
    val distance = calcDistance(node1, node2)
    val direction = calcDirection(node1, node2)
    addFeature("abs(dist)", distance, 0.0)
    addFeature("dist", direction * distance, 0.0)
  }

  /** Number of tokens (plus one) between the two concepts’ spans (zero if the same) */
  def ffDistance {
    val distance = calcDistance(node1, node2)
    val pathStrv3 = depPathStrv3(node1, node2)
    addFeature("d=" + min(distance, 20).toString, 1.0, 1.0)
    addFeature("d=" + min(distance, 20).toString + "+" + pathStrv3, 1.0, 1.0)
  }

  def fflogDistance: Unit = {
    val distance = calcDistance(node1, node2)
    val pathStrv3 = depPathStrv3(node1, node2)
    addFeature("logD", log(distance + 1), log(distance + 1))
    addFeature("logD+" + pathStrv3, log(distance + 1), log(distance + 1))
  }

  def ffDistanceIntervals: Unit = {
    val distance = calcDistance(node1, node2)
    val direction = calcDirection(node1, node2)
    val pathStrv3 = depPathStrv3(node1, node2)
    for (i <- Range(1, distance - 1)) {
      addFeature("dint=" + (direction * i).toString, 1.0, 1.0)
      addFeature("dint=" + (direction * i).toString + "+" + pathStrv3, 1.0, 1.0)
    }
  }

  def fflogDistanceIntervals: Unit = {
    val distance = calcDistance(node1, node2)
    val direction = calcDirection(node1, node2)
    val pathStrv3 = depPathStrv3(node1, node2)
    for (i <- Range(1, Math.floor(log(distance + 1) / log(1.39)).toInt - 1)) {
      addFeature("logDi=" + (direction * i).toString, 1.0, 1.0)
      addFeature("logDi=" + (direction * i).toString + "+" + pathStrv3, 1.0, 1.0)
    }
  }

  def ffDirection: Unit = {
    val dir = calcDirection(node1, node2, extra = .01f)
    addFeature("dir", dir, dir)
  }

  def ffConceptBigram: Unit = {
    // TODO: check that adding no label helps
    addFeature("C1=" + node1.concept + "+C2=" + node2.concept, /*1.0*/ 0.0, 1.0)
  }

  def ffConceptUnigramWithLabel: Unit = {
    addFeature("C1=" + node1.concept, 0.0, 1.0)
    addFeature("C2=" + node2.concept, 0.0, 1.0)
  }

  def ffSRL: Unit = {
    for ((predType, argLabel) <- srl.relations((node1SpanStart, node1SpanEnd), (node2SpanStart, node2SpanEnd))) {
      addFeature("SRL", 1.0, 1.0)
      addFeature("SRL" + predType, 1.0, 1.0)
      addFeature("SRL=" + argLabel, 1.0, 1.0)
      addFeature("SRL" + predType + "=" + argLabel, 1.0, 1.0)
    }
  }

  def ffPosPathUnigramBigramv1: Unit = {
    val posSet1 = posSpanSet(node1).mkString("_")
    val posSet2 = posSpanSet(node2).mkString("_")
    val (unigrams, bigrams) = posPathUnigramAndBigramCounts(node1, node2)
    val pp = "PPv1"
    val direction = if (node1SpanStart < node2SpanStart) {
      "+1"
    } else {
      "-1"
    }
    val dpStr = depPathStrv3(node1, node2)
    for ((unigram, count) <- unigrams) {
      val ppStr = "C1PS=" + posSet1 + "+C2PS=" + posSet2 + "+dir=" + direction + "+" + pp + "U=" + unigram
      addFeature(ppStr, count, count)
      addFeature(ppStr + "_" + count.toString, 1.0, 1.0)
      addFeature(dpStr + "+" + ppStr, count, count)
      addFeature(dpStr + "+" + ppStr + "_" + count.toString, 1.0, 1.0)
    }
    for ((bigram1, bigram2, count) <- bigrams) {
      val ppStr = "C1PS=" + posSet1 + "+C2PS=" + posSet2 + "+dir=" + direction + "+" + pp + "B=" + bigram1 + "_" + bigram2
      addFeature(ppStr, count, count)
      addFeature(ppStr + "_" + count.toString, 1.0, 1.0)
      addFeature(dpStr + "+" + ppStr, count, count)
      addFeature(dpStr + "+" + ppStr + "_" + count.toString, 1.0, 1.0)
    }
  }

  def ffPosPathUnigramBigramv2 {
    val posSet1 = posSpanSet(node1).mkString("_")
    val posSet2 = posSpanSet(node2).mkString("_")
    val (unigrams, bigrams) = posPathUnigramAndBigramCounts(node1, node2)
    val pp = "PPv2"
    val direction = if (node1SpanStart < node2SpanStart) {
      "+1"
    } else {
      "-1"
    }
    val dpStr = depPathStrv3(node1, node2)
    for ((unigram, count) <- unigrams) {
      val ppStr = "C1PS=" + posSet1 + "+C2PS=" + posSet2 + "+dir=" + direction + "+" + pp + "U=" + unigram
      addFeature(ppStr + "_" + count.toString, 1.0, 1.0)
    }
    for ((bigram1, bigram2, count) <- bigrams) {
      val ppStr = "C1PS=" + posSet1 + "+C2PS=" + posSet2 + "+dir=" + direction + "+" + pp + "B=" + bigram1 + "_" + bigram2
      addFeature(ppStr + "_" + count.toString, 1.0, 1.0)
    }
  }

  def ffPosPathUnigramBigramv3 {
    val posSet1 = posSpanSet(node1).mkString("_")
    val posSet2 = posSpanSet(node2).mkString("_")
    val direction = if (node1SpanStart < node2SpanStart) {
      "+1"
    } else {
      "-1"
    }
    val dpStr = depPathStrv3(node1, node2)
    val span1 = fullPos.annotationSpan((node1SpanStart, node1SpanEnd))

    val span2 = fullPos.annotationSpan((node2SpanStart, node2SpanEnd))
    val (start, end) = if (span2._1 - span1._2 >= 0) {
      (span1._2, span2._1)
    } else {
      (span2._2, span1._1)
    }

    val pp = "PPv3"
    val posPath = fullPos.annotation.slice(start, end)
    val ppStrBase = "C1PS=" + posSet1 + "+C2PS=" + posSet2 + "+dir=" + direction + "+" + pp
    val ppStr1 = ppStrBase + ".1=" + posPath.mkString("_")
    val ppStr2 = ppStrBase + ".2=" + posPath.distinct.mkString("_")

    addFeature(ppStr1, 1.0, 1.0)
    addFeature(ppStr1 + dpStr, 1.0, 1.0)
    addFeature(ppStr2, 1.0, 1.0)
    addFeature(ppStr2 + dpStr, 1.0, 1.0)
  }

  def posPathUnigramAndBigramCounts(node1: Node, node2: Node): (List[(String, Int)], List[(String, String, Int)]) = {
    val span1 = fullPos.annotationSpan((graph.spans(node1.spans(0)).start, graph.spans(node1.spans(0)).end))
    val span2 = fullPos.annotationSpan((graph.spans(node2.spans(0)).start, graph.spans(node2.spans(0)).end))
    val (start, end) = if (span2._1 - span1._2 >= 0) {
      (span1._2, span2._1)
    } else {
      (span2._2, span1._1)
    }
    val posPath = fullPos.annotation.slice(start, end).toList
    // posPath must be a list otherwise bigramCounts won't work (equality test fails when using arrays)
    val posPathBigrams = posPath.sliding(2).toList
    val unigramCounts = posPath.distinct.map(x => (x, posPath.count(_ == x)))
    val bigramCounts = posPathBigrams.distinct.map(x => (x.getOrElse(0, "NONE"), x.getOrElse(1, "NONE"), posPathBigrams.count(_ == x)))
    return (unigramCounts, bigramCounts)
  }
  
  /** The pos labels for the node's span */
  def posSpan(node: Node): List[String] = {
    val nodeSpanInGraph = graph.spans(node.spans(0))
    val span = fullPos.annotationSpan((nodeSpanInGraph.start, nodeSpanInGraph.end))
    fullPos.annotation.slice(span._1, span._2).toList
  }

  /** The set of pos labels for the node's span (as a sorted list) */
  def posSpanSet(node: Node): List[String] = {
    posSpan(node).sorted.distinct
  }

  /** Returns the node's span in the dependency parse */
  def getNodeDependencySpan(node: Node): Range = {
    // If an array index out of bounds error occurs in the line below, it could be because the input sentence has a training newline (I know, fragile!)
    val spans = graph.spans(node.spans(0))
    val span = dependencies.annotationSpan((spans.start, spans.end))
    Range(span._1, span._2)
  }

  // TODO: could also do all paths instead of just the shortest
  private def getShortestDependencyPath(nodeA: Node, nodeB: Node): (Int, Int, (List[Int], List[Int])) = {
    val span1 = getNodeDependencySpan(nodeA)
    val span2 = getNodeDependencySpan(nodeB)
    val triples = for {w1 <- span1; w2 <- span2} yield {
      (w1, w2, getDependencyPathBetweenWords(w1, w2))
    }
    triples.minBy(x => x._3._1.size + x._3._2.size)
  }

  /** returns 2 lists:
    * List one is path from word1 to common head;
    * List two is path from common head to word2;
    * Includes the common head in both lists */
  def getDependencyPathBetweenWords(word1: Int, word2: Int): (List[Int], List[Int]) = {
    val path1 = rootDependencyPaths(word1)
    val path2 = rootDependencyPaths(word2)
    val prefix = path1.longestCommonPrefixLength(path2)

    (
      path1.drop(prefix - 1).reverse,
      path2.drop(prefix - 1)
    )
  }

  // TODO: NAUMENKO: why this replaces?? There cant be such parts of speach (VB.* / NN.*)
  private def normalizeVBAndNN(str: String): String = {
    str.replaceAll("VB.*", "VB").replaceAll("NN.*", "NN")
  }

  private def normalizeVBAndNN2(str: String): String = {
    str.replaceAll("VB.*", "VB").replaceAll("NN.*|PRP|FW", "NN").replaceAll("JJ.*", "JJ").replaceAll("RB.*", "RB")
  }

  def ffDependencyPathv2 = {
    val (word1Index, word2Index, path) = getShortestDependencyPath(node1, node2)
    val (word1, word2) = (dependencies.tok(word1Index), dependencies.tok(word2Index))
    val posAnnotations = fullPos.annotation.map(normalizeVBAndNN2)

    val dp = "DPv2="
    if (path._1.size + path._2.size <= 4) {
      val pathStr = dependencyPathString(path, posAnnotations).mkString("_")
      addFeature(dp + pathStr, 0.0, 1.0)
      addFeature("C1=" + node1.concept + "+C2=" + node2.concept + "+" + dp + pathStr, 0.0, 1.0)
      addFeature("W1=" + word1 + "+W2=" + word2 + "+" + dp + pathStr, 0.0, 1.0)
      addFeature("W1=" + word1 + "+" + dp + pathStr, 0.0, 1.0)
      addFeature("W2=" + word2 + "+" + dp + pathStr, 0.0, 1.0)
    }
  }

  def ffDependencyPathvN(prefixN: Int, threashold: Int, unconjoined: Double, conjoined: Double): Unit = {
    ffDependencyPathvN(prefixN, threashold, unconjoined, conjoined, conjoined)
  }

  def ffDependencyPathvN(prefixN: Int, threashold: Int,
                         unconjoined: Double, conjoined: Double, conjoined2: Double) {
    val (word1Index, word2Index, path) = getShortestDependencyPath(node1, node2)
    val (word1, word2) = (dependencies.tok(word1Index), dependencies.tok(word2Index))
    val posAnnotations = fullPos.annotation.map(normalizeVBAndNN)

    val u = unconjoined
    val c = conjoined
    val c2 = conjoined2

    val dpvN = s"DPv$prefixN="
    if (path._1.size + path._2.size <= threashold) {
      // TODO: max path longer
      val pathStr = dependencyPathString(path, posAnnotations).mkString("_")
      addFeature(dpvN + pathStr, u, c2)
      addFeature("C1=" + node1.concept + "+" + dpvN + pathStr, u, c)
      addFeature("C2=" + node2.concept + "+" + dpvN + pathStr, u, c)
      addFeature("W1=" + word1 + "+" + dpvN + pathStr, u, c)
      addFeature("W2=" + word2 + "+" + dpvN + pathStr, u, c)
    } else {
      addFeature(dpvN + "NONE", u, c)
    }
  }

  def ffDependencyPathv3 {
    ffDependencyPathvN(3, 4, 1.0, 0.0, 1.0)
    // NOTE: NAUMENKO: in original threashold  was 5
    //ffDependencyPathvN(3, 5, 1.0, 0.0, 1.0)
  }

  def ffDependencyPathv4 {
    ffDependencyPathvN(4, 5, 1.0, 1.0)
    // NOTE: NAUMENKO: in original threashold  was 6
    //ffDependencyPathvN(4, 6, 1.0, 1.0)
  }

  def ffDependencyPathv5 {
    ffDependencyPathvN(5, 6, 1.0, 1.0)
  }

  // same code as above, just only computes pathStr
  def depPathStrv3(node1: Node, node2: Node): String = {
    val (_, _, path) = getShortestDependencyPath(node1, node2)
    val posAnnotations = fullPos.annotation.map(normalizeVBAndNN)
    val pathStr = if (path._1.size + path._2.size <= 4) {
      dependencyPathString(path, posAnnotations).mkString("_")
    } else {
      "NONE"
    }
    "DPv3=" + pathStr
  }

  // TODO: ffDependencyAllPaths

  def dependencyPathString(path: (List[Int], List[Int]), posAnnotations: Array[String]): List[String] = {
    // Assumes that the POS tags use the same tokenization as the dependencies
    var pathList: List[String] = List()
    for (List(word1, word2) <- path._1.sliding(2)) {
      pathList = posAnnotations(word1) + "_" + dependencies.annotations.find(x => (x.dependent == word1 && x.head == word2)).get.relation + ">_" + posAnnotations(word2) :: pathList
    }
    for (List(word1, word2) <- path._2.sliding(2)) {
      pathList = posAnnotations(word1) + "_" + dependencies.annotations.find(x => (x.head == word1 && x.dependent == word2)).get.relation + "<_" + posAnnotations(word2) :: pathList
    }
    return pathList.reverse
  }

  def dependencyPathStringv2(path: (List[Int], List[Int]), posAnnotations: Array[String]): List[String] = {
    // Assumes that the POS tags use the same tokenization as the dependencies
    // Includes prepositions in the path
    var pathList = List[String]()

    def posAnnotation(word: Int): String = {
      if (posAnnotations(word) == "IN") {
        val (start, stop) = dependencies.getSpan(word, word + 1)
        sentence.slice(start, stop).mkString("_").toLowerCase
      } else {
        posAnnotations(word)
      }
    }

    for (List(word1, word2) <- path._1.sliding(2)) {
      //logger(2, "Looking for dependent="+word1.toString+" head="+word2.toString)
      pathList = posAnnotation(word1) + "_" + dependencies.annotations.find(x => (x.dependent == word1 && x.head == word2)).get.relation + ">_" + posAnnotation(word2) :: pathList
    }
    for (List(word1, word2) <- path._2.sliding(2)) {
      //logger(2, "Looking for dependent="+word2.toString+" head="+word1.toString)
      pathList = posAnnotation(word1) + "_" + dependencies.annotations.find(x => (x.head == word1 && x.dependent == word2)).get.relation + "<_" + posAnnotation(word2) :: pathList
    }
    return pathList.reverse
  }

  private def findRootDependencyPath(wordId: Int, path: List[Int] = List()): List[Int] = {
    // Returns path to root as a list in reverse order (including the word we started at)
    if (wordId == -1) {
      path
    } else {
      val dep = dependencies.annotations.find(_.dependent == wordId)
      if (dep.isEmpty) {
        logger(0, " *** WARNING: The dependency tree seems broken.  I can't find the head of " + input.dependencies.tok(wordId) + " in position " + wordId)
        List()
      } else {
        findRootDependencyPath(dep.get.head, wordId :: path)
      }
    }
  }

  /* *********************** Note ***********************
      All root features should add "+L=<ROOT" to the string
      and do addFeature(featstring, 1.0, 0.0)
      (i.e. leave the unconjoined features blank)
     ************************************************** */
  def ffRootConcept = {
    addFeature("C=" + node1.concept + "+L=<ROOT>", 1.0, 0.0)
  }

  /** Used for cost augmented decoding */
  def ffRootCostAug {
    addFeature("CA:C1=" + node1.concept + "+L=<ROOT>", 1.0, 0.0)
  }

  def ffRootDependencyPathv1 = {
    if (graph.spans.nonEmpty && node1.spans.nonEmpty) {
      val depPath = getNodeDependencySpan(node1).map(w => rootDependencyPaths(w)).minBy(_.size)
      val path = dependencyPathString((List(), depPath), fullPos).mkString("_")
      addFeature("DPRv1=" + path + "+L=<ROOT>", 1.0, 0.0)
    }
  }
}

