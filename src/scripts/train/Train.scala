package scripts.train

import java.io.{FileInputStream, PrintStream}
import java.util.logging.FileHandler

import edu.cmu.lti.nlp.amr.ExtractConceptTable
import scripts.preprocess.Preprocessor
import scripts.utils.TimeUtils.time
import scripts.utils.context.{Context, ContextBuilder, ContextLike}
import scripts.utils.logger.SimpleLoggerLike
import scripts.utils.{AMRParserRunner, FileUtils, TimeUtils}

import scala.io.Source

// Analogue of TRAIN.sh
case class Train(context: Context) extends ContextLike(context) with Runnable with SimpleLoggerLike {

  def run(): Unit = {
    FileUtils.mkDir(context.modelFolder)
    FileUtils.saveFiles(context.modelFolder, "old", Seq("RESULTS.txt"))
    logger.addHandler(new FileHandler(s"${context.modelFolder}/Train.log"))

    time(logger) {
      Preprocessor(context).run()
    }

    logger.info("Make concept table")
    time(logger) {
      createTrainConceptTable()
      countWordFrequences()
    }

    logger.info("Train core start")
    time(logger) {
      runTrainingStage1()
      runTrainingStage2()
    }

    time(logger) {
      SmatchEvaluator(context).runSmatchEvaluations()
    }
  }

  // If there are some results file in model folder, save them it the inner folder


  // Analogue of mkConceptTable
  def createTrainConceptTable(): Unit = {
    logger.info("Create concept table")

    //val in = new FileInputStream(s"$inputFileName.aligned.concepts_no_opN")
    val in = new FileInputStream(s"${context.trainFile}.aligned")
    val out = new PrintStream(s"${context.modelFolder}/conceptTable.train")
    val err = new PrintStream(s"${context.modelFolder}/conceptTable.train.err")
    try {
      val makeConceptTable = new ExtractConceptTable(
        stage1Features = context.stage1Features,
        maxTrainingInstances = 10000,
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