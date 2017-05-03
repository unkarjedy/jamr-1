package scripts.train

import scripts.preprocess.Preprocessor
import scripts.utils.context.{Context, ContextBuilder}

object RunTrain {
  val runProperties = new RunProperties()
  runProperties.load(this.getClass.getClassLoader.getResourceAsStream("run.properties"))

  val jamrRoot = runProperties.jamrRoot
  val corpusFolder = runProperties.corpusFolder
  val corpusFileBaseName = runProperties.corpusFileBaseName
  val modelFolderSuffix = runProperties.modelFolderSuffix

  val stage1Features = List(
    "bias", "length", "firstMatch", "numberIndicator", "badConcept",
    "sentenceMatch", "andList", "pos", "posEvent",
    "phrase", "phraseConceptPair", "phraseConceptPairPOS",
    // Features extracted in ExtractConceptTable (before training step)
    "corpusIndicator", "corpusLength", "count", "conceptGivenPhrase", "phraseGivenConcept"
  ).distinct

  val stage2Features = List(
    "bias", "typeBias", "self", "fragHead", "edgeCount", "distance", "logDistance",
    "rootConcept", "rootDependencyPathv1",
    "conceptBigram",
    "posPathv3", "dependencyPathv4", "dependencyPathv5"
  ).distinct


  val stage1SyntheticConcepts = List(
    "NER", "DateExpr", "OntoNotes", "verbs", "nominalizations",
    "NEPassThrough", "PassThrough", "WordNetPassThrough"
  ).distinct

  def main(args: Array[String]): Unit = {
    val baseInputDataDir = s"$jamrRoot/data/$corpusFolder/"
    val modelFolder = s"$jamrRoot/models/$corpusFolder$modelFolderSuffix"

    val context = ContextBuilder.createContextForTraining(jamrRoot, baseInputDataDir, corpusFileBaseName, modelFolder)
    context.runProperties = runProperties
    context.stage1Features = stage1Features
    context.stage2Features = stage2Features

    context.parserOptions = buildParserOptions(context, stage1SyntheticConcepts)
    context.conceptIdTrainingOptions = buildConceptIdTrainingOptions(trainingLoss = "Infinite_Ramp")
    context.relationIdTrainingOptions = buildRelationIdTrainingOptions()

    Preprocessor(context).run()
    Train(context).run()
  }

  private def runLeaveOneFeatureOut(baseInputDataDir: String, corpus: Corpus) = {
    stage1Features.foreach(featureToRemove => {
      val modelFolder = s"$jamrRoot/models/${corpus.name}_LOO/$featureToRemove"
      val context = ContextBuilder.createContextForTraining(
        jamrRoot,
        baseInputDataDir,
        corpus.baseFileName,
        modelFolder = modelFolder
      )

      val stage1FeaturesWithoutOne = stage1Features.filter(_ != featureToRemove)

      context.stage1Features = stage1FeaturesWithoutOne
      context.stage2Features = stage2Features
      context.parserOptions = buildParserOptions(context, stage1SyntheticConcepts)
      context.conceptIdTrainingOptions = buildConceptIdTrainingOptions(trainingLoss = "Infinite_Ramp")
      context.relationIdTrainingOptions = buildRelationIdTrainingOptions()

      Train(context).run()
    })
  }

  private def buildParserOptions(context: Context,
                                 stage1SyntheticConcepts: List[String],
                                 stage1Only: Boolean = false) = {
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
       |${if (stage1Only) "--stage1-only" else ""}
       """.stripMargin
  }

  private def buildConceptIdTrainingOptions(trainingLoss: String = "Infinite_Ramp"): String = {
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

  private def buildRelationIdTrainingOptions(): String = {
    """--training-optimizer Adagrad
      |--training-passes 10
      |--training-save-interval 1
    """.stripMargin
  }
}

