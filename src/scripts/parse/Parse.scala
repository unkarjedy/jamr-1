package scripts.parse

import java.io.{File, PrintWriter}

import com.typesafe.scalalogging.slf4j.StrictLogging
import scripts.preprocess.Preprocessor
import scripts.utils.context.{Context, ContextLike}
import scripts.utils.logger.SimpleLoggerLike
import scripts.utils.{AMRParserRunner, FileUtils, StageRunnerLike}

import scala.io.Source

// Analogue of PARSE.sh
case class Parse(context: Context,
                 inputFolder: String,
                 inputFileName: String,
                 outputFolder: String) extends ContextLike(context) with Runnable with StageRunnerLike {
  private val inputFile = new File(inputFolder).toPath.resolve(inputFileName).toFile

  override def run(): Unit = {
    FileUtils.mkDir(outputFolder)

    runStage("### Preprocess input sentences ###", context.runProperties.skipPreprocessing) {
      proceedPreprocessing()
    }

    tryRunStage("### Running JAMR Parser ###", skip = runProperties.skipParse) {
      proceedParse()
    }

//    removeTempFiles(inputFolder)
  }

  private def proceedPreprocessing() = {
    val preprocessor = Preprocessor(context)

    runStage("### Tokenizing input ###", runProperties.skipTokenizer) {
      runTokenizer()
    }

    runStage("### Running NER system ###", runProperties.skipNER){
      preprocessor.runIllinoisNamedEntityTagger(inputFile, System.err, System.err) // redirect both to err
    }

    runStage("### Running dependency parser ###", runProperties.skipDEP) {
      preprocessor.runStandfordDependencyParser(inputFile, s"$inputFile.deps")
    }
  }

  private def runTokenizer(): Unit = {
    val tokTmp = s"$inputFile.tok.tmp"
    val sntWriter = new PrintWriter(tokTmp)
    InputSentencesReader.getStream(Source.fromFile(inputFile))
      .map(_.sentence.replaceAll("\\s+", " "))
      .foreach(sntWriter.println)
    sntWriter.close()

    val pb = new ProcessBuilder("sh", s"${context.cdecPath}/corpus/tokenize-anything.sh")
    pb.directory(new File(inputFolder))
    pb.redirectInput(new File(tokTmp))
    pb.redirectOutput(new File(s"$inputFile.tok"))
    pb.redirectError(new File(s"$inputFile.tok.err"))

    val proc = pb.start()
    val returnCode = proc.waitFor()
    logger.info(s"tokenize-anything.sh  returned code: $returnCode")
    if(returnCode != 0) {
      throw new RuntimeException(s"tokenize-anything.sh exited with error code: ${returnCode}")
    }
  }

  private def proceedParse(): Unit = {
    val outputPath = new File(outputFolder).toPath

    val args =
      s"""--stage1-concept-table "${context.modelFolder}/conceptTable.train"
         |--stage1-weights "${context.stage1Weights}"
         |--stage2-weights "${context.stage2Weights}"
         |--dependencies "$inputFile.deps"
         |--dependencies-kbest "$inputFile.deps.k_best.txt"
         |--ner "$inputFile.IllinoisNER"
         |--tok "$inputFile.tok"
         |--progress-file "${outputPath.resolve("parse-progress.txt").toString}"
         |${context.parserOptions}
      """.stripMargin

    AMRParserRunner.run(
      argsString = args,
      inFilePath = inputFile.getPath,
      outFilePath = outputPath.resolve(s"$inputFileName.out").toString,
      errFilePath = outputPath.resolve(s"$inputFileName.err").toString
    )
  }

  private def removeTempFiles(folder: String) = {
    val tmpFiles = new File(folder).listFiles().filter(_.isFile)
      .filter(_.getName.endsWith(".tmp"))
    tmpFiles.foreach(_.delete())
  }
}



