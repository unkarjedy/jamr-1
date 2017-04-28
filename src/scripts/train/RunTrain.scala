package scripts.train

import scripts.preprocess.Preprocessor
import scripts.utils.context.{Context, ContextBuilder}

object RunTrain {
  private val jamrRoot = "C:/Users/Dmitrii_Naumenko/Desktop/JAMR_2016_Github/"

  val stage1Features = List(
    "bias", "corpusIndicator", "length", "corpusLength", "conceptGivenPhrase", "count",
    "phraseGivenConcept", "phraseConceptPair", "phrase", "firstMatch", "numberIndicator",
    "sentenceMatch", "andList", "pos", "posEvent", "phraseConceptPairPOS", "badConcept"
  )
  val stage2Features = List(
    "rootConcept", "rootDependencyPathv1", "bias", "typeBias",
    "self", "fragHead", "edgeCount", "distance", "logDistance",
    "posPathv3", "dependencyPathv4", "conceptBigram", "dependencyPathv5"
  )

  def main(args: Array[String]): Unit = {
    val corpus = Corpus.Little_Prince_v1_6
    val baseInputDataDir = s"$jamrRoot/data/${corpus.name}/"

    val context = ContextBuilder.createContextForTraining(jamrRoot,
                                                          baseInputDataDir,
                                                          corpus.baseFileName,
                                                          modelFolder = "")
//    Preprocessor(context).run()

    1 to 3 foreach (iteration => {
      stage1Features.foreach(feature => {
        val stage1FeaturesWithoutOne = stage1Features.filter(_ != feature)

        context.modelFolder = s"$jamrRoot/models/${corpus.name}_LOO/${feature}_$iteration"

        context.stage1Features = stage1FeaturesWithoutOne
        context.stage2Features = stage2Features

        val stage1SyntheticConcepts = List(
          "NER", "DateExpr", "OntoNotes", "NEPassThrough", "PassThrough",
          "WordNetPassThrough", "verbs", "nominalizations"
        )

        context.parserOptions = buildParserOptions(context, stage1SyntheticConcepts)
        context.conceptIdTrainingOptions = buildConceptIdTrainingOptions(trainingLoss = "Infinite_Ramp")
        context.relationIdTrainingOptions = buildRelationIdTrainingOptions()

        Train(context).run()
      })
    })

  }

  private def buildParserOptions(context: Context, stage1SyntheticConcepts: List[String]) = {
    s"""--stage1-synthetic-concepts ${stage1SyntheticConcepts.mkString(",")}
       |--stage1-predicates ${context.jamrRoot}/resources/OntoNotes-v4-predicates.txt
       |--stage1-phrase-counts ${context.modelFolder}/wordCounts.train
       |--stage1-features ${context.stage1Features.mkString(",")}
       |--stage2-decoder LR
       |--stage2-approx-decoder Greedy
       |--stage2-features ${context.stage2Features.mkString(",")}
       |--stage2-labelset ${context.jamrRoot}/resources/labelset-r4
       |--output-format AMR
       |--ignore-parser-errors
       |--print-stack-trace-on-errors
       """.stripMargin
  }

  def buildConceptIdTrainingOptions(trainingLoss: String): String = {
    s"""--training-optimizer Adagrad
       |--training-passes 10
       |--training-stepsize 1
       |--stage1-training-leave-one-out
       |--training-prec-recall .1
       |--training-cost-scale 100
       |--training-loss $trainingLoss
       |--training-l2-strength .00001
       |--training-save-interval 1
       |-v 1
    """.stripMargin
  }

  def buildRelationIdTrainingOptions(): String = {
    """--training-optimizer Adagrad
      |--training-passes 10
      |--training-save-interval 1
    """.stripMargin
  }
}

