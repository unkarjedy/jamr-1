package scripts.train

import java.util.logging.{ConsoleHandler, FileHandler}

import scripts.utils.context.{Context, ContextLike}
import scripts.utils.logger.{SimpleLoggerLike, UnmodifyingFormatter}
import scripts.utils.{AMRParserRunner, StageRunnerLike}

import scala.io.Source
import scala.sys.process.ProcessLogger

// Final part of TRAIN.sh for evaluating trained model using Smatch
case class SmatchEvaluator(context: Context)
  extends ContextLike(context) with SimpleLoggerLike with StageRunnerLike {

  private val settings = SmatchEvaluator.Settings(context)

  // Setup logger to print log to file in addition to console
  logger.setUseParentHandlers(false)
  logger.addHandler(new ConsoleHandler())
  logger.addHandler(new FileHandler(s"${settings.modelFolder}/RESULTS.txt"))
  logger.getHandlers.foreach(_.setFormatter(new UnmodifyingFormatter))

  private val processLogger: ProcessLogger = ProcessLogger(
    out => logger.info(out),
    err => logger.info(err)
  )

  def runSmatchEvaluations(): Unit = {
    logger.info("----- Evaluation on Test: Smatch (all stages) -----")
    runStage("decoding", runProperties.skipEvaluateAllStageDecode) {
      decodeAllStages()
    }
    runStage("evaluating", skip = false) {
      val command = smatchCommand(s"${settings.modelFolder}/test.decode.allstages", settings.testFile)
      sys.process.stringToProcess(command) ! processLogger
    }

    logger.info("")
    logger.info("----- Evaluation on Test: Smatch (gold concept ID) -----")
    runStage("decoding", runProperties.skipEvaluateStage2Decode) {
      decodeStage2()
    }
    runStage("evaluating", skip = false) {
      val command = smatchCommand(s"${settings.modelFolder}/test.decode.stage2only", settings.testFile)
      sys.process.stringToProcess(command) ! processLogger
    }

    logger.info("")
    logger.info("----- Evaluation on Test: Spans -----")
    // NOTE: evaluation for spans results were saved during all stages decoding in the end of .err file
    takeTaleLines(fileName = s"${settings.modelFolder}/test.decode.allstages.err", linesToTake = 6)
  }

  /** Takes `lineToTake` lines from the end of the file content */
  private def takeTaleLines(fileName: String, linesToTake: Int) = {
    val lines = Source.fromFile(fileName).getLines()
    lines.toList
      .reverseIterator.take(linesToTake).toList.reverse
      .filter(_.trim.nonEmpty)
      .foreach(logger.info)
  }

  /**
    * search for smatch_modified.py options:
    * -f Two files containing AMR pairs. AMRs in each file are separated by a single blank line. This option is required.
    * --pr Output precision and recall as well as the f-score. Default: false
    */
  private def smatchCommand(decodingResultFile: String, testFile: String): String = {
    s"""python ${settings.smatchScriptPath} --pr -f "$decodingResultFile" "$testFile""""
  }

  // cmd.test.decode.allstages
  private def decodeAllStages(): Unit = {
    val outputAllStages = s"${settings.modelFolder}/test.decode.allstages"
    runDecodeStagesCommon(outputAllStages, "--stage1-eval")
  }

  // cmd.test.decode.stage2only
  private def decodeStage2(): Unit = {
    val outputStage2 = s"${settings.modelFolder}/test.decode.stage2only"
    runDecodeStagesCommon(outputStage2, "--stage1-oracle")
  }

  private def runDecodeStagesCommon(output: String, stage1Decoder: String): Unit = {
    val input = settings.testFile

    val stage1Weights = s"${settings.modelFolder}/stage1-weights"
    val stage2Weights = s"${settings.modelFolder}/stage2-weights.iter5" // TODO: pick iteration by performance on dev

    val args =
      s"""--stage1-concept-table "${settings.modelFolder}/conceptTable.train"
         |--stage1-weights "$stage1Weights"
         |--stage2-weights "$stage2Weights"
         |--dependencies "$input.snt.deps"
         |--ner "$input.snt.IllinoisNER"
         |--tok "$input.snt.tok"
         |--training-data "$input.aligned.no_opN"
         |$stage1Decoder
         |-v 0
         |${settings.parserOptions}
      """.stripMargin

    AMRParserRunner.run(
      argsString = args,
      inFilePath = s"$input.snt",
      outFilePath = output,
      errFilePath = s"$output.err"
    )
  }

}

object SmatchEvaluator {
  case class Settings(modelFolder: String,
                      testFile: String,
                      smatchScriptPath: String,
                      parserOptions: String)

  object Settings {
    def apply(ctx: Context): Settings = Settings(
      ctx.modelFolder,
      ctx.testFile,
      ctx.smatchPath,
      ctx.parserOptions
    )
  }
}