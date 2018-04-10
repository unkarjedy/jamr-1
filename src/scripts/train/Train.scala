package scripts.train

import java.io.{File, FileInputStream, FileWriter, PrintStream}
import java.util.logging.FileHandler

import com.typesafe.scalalogging.slf4j.StrictLogging
import edu.cmu.lti.nlp.amr.ExtractConceptTable
import scripts.utils.context.{Context, ContextLike}
import scripts.utils.logger.SimpleLoggerLike
import scripts.utils.{AMRParserRunner, FileUtils, StageRunnerLike}

import scala.io.Source

// Analogue of TRAIN.sh
case class Train(context: Context) extends ContextLike(context)
                                           with Runnable
                                           with SimpleLoggerLike
                                           with StageRunnerLike {
  FileUtils.mkDir(context.modelFolder)
  logger.addHandler(new FileHandler(s"${context.modelFolder}/Train.log"))
  initLogger()

  private val CONTEXT_DUMP_FILE_NAME = "context_dump.txt"

  def run(): Unit = {
    // If there are some results file in model folder, save them it the inner folder
    FileUtils.saveFiles(context.modelFolder, "prev_results", Seq("RESULTS.txt", CONTEXT_DUMP_FILE_NAME))

    dumpContext(context, CONTEXT_DUMP_FILE_NAME)

    runStage("Make concept table", runProperties.skipConceptTableExtraction) {
      createTrainConceptTable()
      countWordFrequences()
    }

    logger.info("Train core start")
    runStage("Training stage 1", runProperties.skipTrainStage1) {
      runTrainingStage1()
    }
    runStage("Training stage 2", runProperties.skipTrainStage2) {
      runTrainingStage2()
    }

    runStage("Evaluating results", runProperties.skipEvaluating) {
      SmatchEvaluator(context).runSmatchEvaluations()
    }
  }

  private def dumpContext(context: Context, dumpFileName: String) = {
    val dumpFile = new File(context.modelFolder).toPath.resolve(dumpFileName).toFile
    val writer = new FileWriter(dumpFile)
    writer.write(context.toLogString)
    writer.close()
  }

  // Analogue of mkConceptTable
  def createTrainConceptTable(): Unit = {
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