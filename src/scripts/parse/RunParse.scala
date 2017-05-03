package scripts.parse

import scripts.train.Corpus
import scripts.utils.context.ContextBuilder

object RunParse {
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
    val jamrRoot = "C:/Users/Dmitrii_Naumenko/Desktop/JAMR_2016_Github/"
    val corpus = Corpus.LDC2014T12_PROXY
    val modelFolder = s"$jamrRoot/models/${corpus.name}"

    val context = ContextBuilder.createContext(jamrRoot, modelFolder)

    context.stage1Weights = s"${context.modelFolder}/stage1-weights"
    context.stage2Weights = s"${context.modelFolder}/stage2-weights.iter5"

    //  bias,length,fromNERTagger,conceptGivenPhrase
    context.parserOptions =
      s"""--stage1-phrase-counts ${context.modelFolder}/wordCounts.train
         |--stage1-features ${stage1Features.mkString(",")}
         |--stage2-features ${stage2Features.mkString(",")}
         |--stage2-decoder LR
         |--stage2-labelset $jamrRoot/resources/labelset-r3
         |--output-format AMR,nodes,edges,root
         |--ignore-parser-errors
         |--print-stack-trace-on-errors
         |-v 1
      """.stripMargin

    val inputFolder = s"$jamrRoot/parsing_sandbox"
    val outputFolder = s"$inputFolder/out"
    val inputFilename= "in.txt"
    Parse(context, inputFolder, inputFilename, outputFolder)
      .run()
  }
}
