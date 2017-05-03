package scripts.train

import java.util.logging.{ConsoleHandler, FileHandler}

import scripts.utils.{AMRParserRunner, StageRunnerLike}
import scripts.utils.context.{Context, ContextLike}
import scripts.utils.logger.{SimpleLoggerLike, UnmodifyingFormatter}

import scala.io.Source
import scala.sys.process._

// Final part of TRAIN.sh for evaluating trained model using Smatch
case class SmatchEvaluator(context: Context) extends ContextLike(context)
                                                     with SimpleLoggerLike
                                                     with StageRunnerLike {

  // Setup logger to print log to file in addition to console
  logger.setUseParentHandlers(false)
  logger.addHandler(new ConsoleHandler())
  logger.addHandler(new FileHandler(s"${context.modelFolder}/RESULTS.txt"))
  logger.getHandlers.foreach(_.setFormatter(new UnmodifyingFormatter))

  private val getProcessLogger = ProcessLogger(out => logger.info(out), err => logger.info(err))

  def runSmatchEvaluations(): Unit = {
    logger.info("----- Evaluation on Test: Smatch (all stages) -----")
    runStage("decoding", runProperties.skipEvaluateAllStageDecode) {
      decodeAllStages()
    }
    runStage("evaluating", skip = false) {
      val command1 = smatchCommand(s"${context.modelFolder}/test.decode.allstages", context.testFile)
      stringToProcess(command1) ! getProcessLogger
    }

    logger.info("")
    logger.info("----- Evaluation on Test: Smatch (gold concept ID) -----")
    runStage("decoding", runProperties.skipEvaluateStage2Decode) {
      decodeStage2()
    }
    runStage("evaluating", skip = false) {
      val command2 = smatchCommand(s"${context.modelFolder}/test.decode.stage2only", context.testFile)
      stringToProcess(command2) ! getProcessLogger
    }

    logger.info("")
    logger.info("----- Evaluation on Test: Spans -----")
    // NOTE: evaluation for spans results were saved during all stages decoding
    takeTaleLines(fileName = s"${context.modelFolder}/test.decode.allstages.err", linesToTake = 6)
  }

  /** Takes $linetoTake lines from the end of the file content*/
  private def takeTaleLines(fileName: String, linesToTake: Int) = {
    val lines = Source.fromFile(fileName).getLines()
    lines.toList
      .reverseIterator.take(linesToTake).toList.reverse
      .filter(_.trim.nonEmpty)
      .foreach(logger.info)
  }

  private def smatchCommand(decodingResultFile: String, testFile: String): String = {
    s"""python ${context.smatchPath} --pr -f "$decodingResultFile" "$testFile""""
  }

  // cmd.test.decode.allstages
  private def decodeAllStages(): Unit = {
    val outputAllStages = s"${context.modelFolder}/test.decode.allstages"
    runDecodeStagesCommon(outputAllStages, "--stage1-eval")
  }

  // cmd.test.decode.stage2only
  private def decodeStage2(): Unit = {
    val outputStage2 = s"${context.modelFolder}/test.decode.stage2only"
    runDecodeStagesCommon(outputStage2, "--stage1-oracle")
  }

  private def runDecodeStagesCommon(output: String, stage1Decoder: String): Unit = {
    val input = context.testFile

    val stage1Weights = s"${context.modelFolder}/stage1-weights"
    val stage2Weights = s"${context.modelFolder}/stage2-weights.iter5" // pick iteration by performance on dev

    val args =
      s"""--stage1-concept-table "${context.modelFolder}/conceptTable.train"
         |--stage1-weights "$stage1Weights"
         |--stage2-weights "$stage2Weights"
         |--dependencies "$input.snt.deps"
         |--ner "$input.snt.IllinoisNER"
         |--tok "$input.snt.tok"
         |--training-data "$input.aligned.no_opN"
         |$stage1Decoder
         |-v 0
         |${context.parserOptions}
      """.stripMargin

    AMRParserRunner.run(argsString = args,
                        inFilePath = s"$input.snt",
                        outFilePath = output,
                        errFilePath = s"$output.err")
  }

}
