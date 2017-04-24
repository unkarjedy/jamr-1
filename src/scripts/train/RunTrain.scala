package scripts.train

import scripts.utils.context.ContextBuilder

object RunTrain {

  def main(args: Array[String]): Unit = {
    val jamrRoot = "C:/Users/unkarjedy/Desktop/Jamr_Fork/"
    val corpusName = "amr-bank-struct-v1.6"
    val baseCorpusFileName = "amr-bank-struct-v1.6"
    val baseDataDir = s"$jamrRoot/data/$corpusName/"
    val modelFolder = s"$jamrRoot/models/$corpusName"
    val context = ContextBuilder.createContextForTraining(jamrRoot, baseDataDir, baseCorpusFileName, modelFolder)

    context.stage1Features = List("bias", "corpusIndicator", "length", "corpusLength", "conceptGivenPhrase", "count",
                                  "phraseGivenConcept", "phraseConceptPair", "phrase", "firstMatch", "numberIndicator",
                                  "sentenceMatch", "andList", "pos", "posEvent", "phraseConceptPairPOS", "badConcept")

    val stage1SyntheticConcepts = List("NER", "DateExpr", "OntoNotes", "NEPassThrough", "PassThrough",
                                       "WordNetPassThrough", "verbs", "nominalizations")

    context.parserOptions =
      s"""    --stage1-synthetic-concepts ${stage1SyntheticConcepts.mkString(",")}
         |    --stage1-predicates $jamrRoot/resources/OntoNotes-v4-predicates.txt
         |    --stage1-phrase-counts $modelFolder/wordCounts.train
         |    --stage1-features ${context.stage1Features.mkString(",")}
         |
         |    --stage2-decoder LR
         |    --stage2-approx-decoder Greedy
         |    --stage2-features rootConcept,rootDependencyPathv1,bias,typeBias,self,fragHead,edgeCount,distance,logDistance,posPathv3,dependencyPathv4,conceptBigram,dependencyPathv5
         |    --stage2-labelset $jamrRoot/resources/labelset-r4
         |
         |    --output-format AMR
         |    --ignore-parser-errors
         |    --print-stack-trace-on-errors
       """.stripMargin

    context.conceptIdTrainingOptions =
      """    --training-optimizer Adagrad
        |    --training-passes 10
        |    --training-stepsize 1
        |    --stage1-training-leave-one-out
        |    --training-prec-recall .1
        |    --training-cost-scale 100
        |    --training-loss Infinite_Ramp
        |    --training-l2-strength .00001
        |    --training-save-interval 1
        |    -v 1
      """.stripMargin

    context.relationIdTrainingOptions =
      """--training-optimizer Adagrad
        |--training-passes 10
        |--training-save-interval 1
      """.stripMargin

    Train(context).run()
  }

}

