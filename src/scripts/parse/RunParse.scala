package scripts.parse

import scripts.train.RunProperties
import scripts.utils.context.{Context, ContextBuilder}

object RunParse {
  private val runProperties = new RunProperties("run.properties")

  private val jamrRoot = runProperties.jamrRoot

  private val stage1Features = List(
    "bias", "length", "firstMatch", "numberIndicator", "badConcept",
    "sentenceMatch", "andList", "pos", "posEvent",
//    "phrase", "phraseConceptPair", "phraseConceptPairPOS",
    "corpusIndicator", "corpusLength", "count", "conceptGivenPhrase", "phraseGivenConcept"
  ).distinct

  private val stage2Features = List(
    "bias", "typeBias", "self", "fragHead", "edgeCount", "distance", "logDistance",
    "rootConcept", "rootDependencyPathv1",
    "conceptBigram",
    "posPathv3", "dependencyPathv4", "dependencyPathv5"

  ).distinct

  def main(args: Array[String]): Unit = {
    val modelFolder = s"$jamrRoot/models/${runProperties.modelFolder}"

    val context = ContextBuilder.createContext(jamrRoot, modelFolder)
    context.runProperties = runProperties
    context.stage1Weights = s"${context.modelFolder}/stage1-weights"
    context.stage2Weights = s"${context.modelFolder}/stage2-weights"
//    context.stage2Weights = s"${context.modelFolder}/stage2-weights.iter5"
    context.stage1Features = stage1Features
    context.stage2Features = stage2Features
    context.parserOptions = buildParserOptionsString(jamrRoot, context)

    val inputFilename= runProperties.parserInputFilName
    val inputFolder = s"$jamrRoot/${runProperties.parserInputFolder}"
    val outputFolder = s"$inputFolder/out"
    Parse(context, inputFolder, inputFilename, outputFolder)
      .run()
  }

  private def buildParserOptionsString(jamrRoot: String, context: Context) = {
    s"""--stage1-phrase-counts ${context.modelFolder}/wordCounts.train
       |--stage1-features ${context.stage1Features.mkString(",")}
       |--stage2-features ${context.stage2Features.mkString(",")}
       |--stage2-decoder LR
       |--stage2-labelset $jamrRoot/resources/labelset-r3
       |--output-format AMR,nodes,edges,root
       |--ignore-parser-errors
       |--print-stack-trace-on-errors
       |-v 1
      """.stripMargin
  }
}
