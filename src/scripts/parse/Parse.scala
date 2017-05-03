package scripts.parse

import java.io.{File, PrintWriter}

import scripts.preprocess.Preprocessor
import scripts.utils.context.{Context, ContextBuilder, ContextLike}
import scripts.utils.logger.SimpleLoggerLike
import scripts.utils.{AMRParserRunner, FileUtils}

import scala.io.Source

// Analogue of PARSE.sh
case class Parse(context: Context,
                 inputFolder: String,
                 inputFileName: String,
                 outputFolder: String) extends ContextLike(context) with Runnable with SimpleLoggerLike {
  val inputFile = new File(inputFolder).toPath.resolve(inputFileName).toFile

  override def run(): Unit = {
    FileUtils.mkDir(outputFolder)

    tokenize()
    val preprocessor = Preprocessor(context)

    logger.info("### Running NER system ###")
    preprocessor.runIllinoisNamedEntityTagger(inputFile.getPath, System.err, System.err) // redirect both to err
    logger.info("### Running dependency parser ###")
    preprocessor.runStandfordDependencyParser(inputFile.getPath, s"$inputFile.deps")

    logger.info("### Running JAMR ###")
    proceedParse()

    val tmpFiles = new File(inputFolder).listFiles().filter(_.isFile)
      .filter(_.getName.endsWith(".tmp"))
    tmpFiles.foreach(_.delete())
  }

  private def tokenize(): Unit = {
    val tokTmp = s"$inputFile.tok.tmp"
    val sntWriter = new PrintWriter(tokTmp)
    Source.fromFile(inputFile).getLines()
      .map(_.replaceAll("\\s+", " "))
      .foreach(sntWriter.println)
    sntWriter.close()

    val pb = new ProcessBuilder("sh", s"${context.cdecPath}/corpus/tokenize-anything.sh")
    pb.directory(new File(inputFolder))
    pb.redirectInput(new File(tokTmp))
    pb.redirectOutput(new File(s"$inputFile.tok"))

    val proc = pb.start()
    proc.waitFor()
  }

  private def proceedParse(): Unit = {
    val args =
      s"""--stage1-concept-table "${context.modelFolder}/conceptTable.train"
         |--stage1-weights "${context.stage1Weights}"
         |--stage2-weights "${context.stage2Weights}"
         |--dependencies "$inputFile.deps"
         |--ner "$inputFile.IllinoisNER"
         |--tok "$inputFile.tok"
         |-v 0
         |${context.parserOptions}
      """.stripMargin

    val outputPath = new File(outputFolder).toPath
    AMRParserRunner.run(
      argsString = args,
      inFilePath = inputFile.getPath,
      outFilePath = outputPath.resolve(s"$inputFileName.out").toString,
      errFilePath = outputPath.resolve(s"$inputFileName.err").toString
    )
  }

}



