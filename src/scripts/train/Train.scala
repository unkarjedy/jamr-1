package scripts.train

import java.io.{FileInputStream, PrintStream}

import edu.cmu.lti.nlp.amr.ExtractConceptTable
import scripts.preprocess.Preprocessor
import scripts.utils.context.{Context, ContextBuilder, ContextLike}
import scripts.utils.logger.SimpleLoggerLike
import scripts.utils.{AMRParserRunner, FileUtils}

import scala.io.Source

// Analogue of TRAIN.sh
case class Train(context: Context) extends ContextLike(context) with Runnable with SimpleLoggerLike {

  def run(): Unit = {
    // If there are some results file in model folder, save them it the inner folder
    FileUtils.saveFiles(baseFolder = context.modelFolder,
                        saveToFolder = "old",
                        filesToSave = Seq("RESULTS.txt"))

    Preprocessor(context).run()

    logger.info("Train core start")
    createTrainConceptTable()
    countWordFrequences()
    runTrainingStage1()
    runTrainingStage2()

    SmatchEvaluator(context).runSmatchEvaluations()
  }

  // Analogue of mkConceptTable
  def createTrainConceptTable(): Unit = {
    logger.info("Create concept table")

    //val in = new FileInputStream(s"$inputFileName.aligned.concepts_no_opN")
    val in = new FileInputStream(s"${context.trainFile}.aligned")
    val out = new PrintStream(s"${context.modelFolder}/conceptTable.train")
    val err = new PrintStream(s"${context.modelFolder}/conceptTable.train.err")
    try {
      val makeConceptTable = new ExtractConceptTable(
        stage1Features = context.stage1Features.split(",").toList,
        maxTrainingInstances = 1000,
        in, out, err
      )
      makeConceptTable.run()
    } finally {
      in.close()
      out.close()
    }
  }

  def countWordFrequences(): Unit = {
    val in = new FileInputStream(s"${context.trainFile}.snt.tok")
    val out = new PrintStream(s"${context.modelFolder}/wordCounts.train")

    try {
      val wordCounts: Map[String, Int] = Source.fromInputStream(in)
        .getLines
        .flatMap(_.split("\\s+"))
        .foldLeft(Map.empty[String, Int]) {
          (count, word) => count + (word -> (count.getOrElse(word, 0) + 1))
        }

      wordCounts.toSeq.sortBy(t => (t._2, t._1)).reverse.foreach {
        case (word, count) => out.println(s"$word $count")
      }
    } finally {
      in.close()
      out.close()
    }
  }

  // cmd.stage1-weights
  def runTrainingStage1(): Unit = {
    logger.info("Training stage 1")

    val input = context.trainFile
    val output = s"${context.modelFolder}/stage1-weights"

    val args =
      s"""--stage1-train
         |--stage1-concept-table "${context.modelFolder}/conceptTable.train"
         |--tok "$input.snt.tok"
         |--snt "$input.snt"
         |--dependencies "$input.snt.deps"
         |--ner "$input.snt.IllinoisNER"
         |
         |--training-dev "${context.devFile}"
         |--training-output "$output"
         |--output-format AMR
         |
         |-v -1
         |${context.parserOptions}
         |${context.conceptIdTrainingOptions}
      """.stripMargin

    AMRParserRunner.run(argsString = args,
                        inFilePath = s"$input.aligned.no_opN",
                        outFilePath = output,
                        errFilePath = s"$output.err")
  }

  // cmd.stage2-weights
  def runTrainingStage2(): Unit = {
    logger.info("Training stage 2")

    val input = context.trainFile
    val output = s"${context.modelFolder}/stage2-weights"

    val args =
      s"""--stage2-train
         |--stage1-concept-table "${context.modelFolder}/conceptTable.train"
         |--tok "$input.snt.tok"
         |--snt "$input.snt"
         |--dependencies "$input.snt.deps"
         |--ner "$input.snt.IllinoisNER"
         |
         |--training-dev "${context.devFile}"
         |--training-output "$output"
         |--output-format triples
         |--smatch-eval "${context.smatchPath}"
         |
         |-v 0
         |${context.parserOptions}
         |${context.relationIdTrainingOptions}
      """.stripMargin

    AMRParserRunner.run(argsString = args,
                        inFilePath = s"$input.aligned.no_opN",
                        outFilePath = output,
                        errFilePath = s"$output.err")
  }

}

object Train {

  def main(args: Array[String]): Unit = {
    val jamrRoot = "C:/Users/unkarjedy/Desktop/Jamr_Fork"
    val baseDataDir = s"$jamrRoot/data/AMR-Bank-v1.6_SCALA/"
    val baseDataFileName = "amr-bank-struct-v1.6"
    val modelFolder = s"$jamrRoot/models/Little_Prince_SCALA"

    val context = ContextBuilder.createContextForTraining(jamrRoot, baseDataDir, baseDataFileName, modelFolder)

    context.stage1Features = "bias,corpusIndicator,length,corpusLength,conceptGivenPhrase,count,phraseGivenConcept,phraseConceptPair,phrase,firstMatch,numberIndicator,sentenceMatch,andList,pos,posEvent,phraseConceptPairPOS,badConcept"

    context.parserOptions =
      s"""
         |   --stage1-synthetic-concepts NER,DateExpr,OntoNotes,NEPassThrough,PassThrough,WordNetPassThrough,verbs,nominalizations
         |    --stage1-predicates $jamrRoot/resources/OntoNotes-v4-predicates.txt
         |    --stage1-phrase-counts $modelFolder/wordCounts.train
         |    --stage1-features ${context.stage1Features}
         |    --stage2-decoder LR
         |    --stage2-approx-decoder Greedy
         |    --stage2-features rootConcept,rootDependencyPathv1,bias,typeBias,self,fragHead,edgeCount,distance,logDistance,posPathv3,dependencyPathv4,conceptBigram,dependencyPathv5
         |    --stage2-labelset $jamrRoot/resources/labelset-r4
         |    --output-format AMR
         |    --ignore-parser-errors
         |    --print-stack-trace-on-errors
       """.stripMargin

    context.conceptIdTrainingOptions =
      """   --training-optimizer Adagrad
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
