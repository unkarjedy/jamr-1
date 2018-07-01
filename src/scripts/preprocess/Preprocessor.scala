package scripts.preprocess

import java.io._
import java.nio.charset.StandardCharsets

import edu.cmu.lti.nlp.amr.align.Aligner
import edu.cmu.lti.nlp.amr.standford_parser.RunStanfordParser
import edu.cmu.lti.nlp.amr.{CorpusTool, IllinoisNERConvert}
import scripts.parse.InputSentencesReader
import scripts.utils.context.{Context, ContextBuilder, ContextLike}
import scripts.utils.{StageRunnerLike, StreamUtils}
import term_dict_process.Utils

import scala.io.Source

// Analogue of PREPROCESS.sh
// Proceed preprocessing of dev, test, train files: extracts sentences, tokens, NamedEntities, Standford Dependencies
case class Preprocessor(ctx: Context) extends ContextLike(ctx)
  with Runnable
  with StageRunnerLike {

  override def run(): Unit = {
    runStage("Preprocessing", runProperties.skipPreprocessing) {
      proceedPreprocess()
      logger.info("Preprocessing end")
    }
  }

  private def proceedPreprocess(): Unit = {
    val files = Seq(ctx.devFile, ctx.trainFile, ctx.testFile).map(new File(_))
    files.foreach(file => {
      extractSentencesAndTokens(file)
      runAligner(file)
    })

    extractConceptsNoOpN(ctx.trainFile)

    files.foreach(file => {
      runStandfordDependencyParser(file)
      runIllinoisNamedEntityTagger(file)
    })
  }

  def extractSentencesAndTokens(amrFile: File): Unit = {
    val amrFilePath = amrFile.getPath
    val sntOutputFile = new File(s"$amrFilePath.snt")
    val sntTokOutputFile = new File(s"$amrFilePath.snt.tok")
    val tokOutputFile = new File(s"$amrFilePath.tok")

    //  ./cmd.snt
    val sntPrintStream = new PrintStream(sntOutputFile)
    Source.fromFile(amrFile).getLines()
      .filter(_.startsWith("# ::snt "))
      .map(_.replaceAll("^# ::snt ", "").replaceAll("  +", " "))
      .foreach(sntPrintStream.println)
    sntPrintStream.close()

    // ./cmd.snt.tok
    val pb = new ProcessBuilder("sh", s"${ctx.cdecPath}/corpus/tokenize-anything.sh")
    pb.directory(amrFile.getParentFile)
    pb.redirectInput(sntOutputFile)
    pb.redirectOutput(sntTokOutputFile)
    pb.redirectError(sntTokOutputFile)
    val proc = pb.start()
    proc.waitFor()

    // ./cmd.tok
    val in = new FileInputStream(amrFile)
    val out = new PrintStream(tokOutputFile)
    try {
      val tool = new CorpusTool(in, out, sntTokOutputFile.getPath)
      tool.run()
    } finally {
      in.close()
      out.close()
    }
  }

  // ./cmd.aligned command analogue.
  // this works a bit differently.
  // it does not print out and err stream in one messy .aligned.log file and then separates amr data and other trash
  // it prints amr data and other separately in out and err
  def runAligner(amrFile: File): Unit = {
    val amrFileName = amrFile.getName

    // ./cmd.aligned
    logger.info(s"Aligning sentences from $amrFileName")
    val tokFile = new File(s"${amrFile.getPath}.tok")
    val alignedFile = new File(s"${amrFile.getPath}.aligned")
    val alignedLogFile = new File(s"${amrFile.getPath}.aligned.log")

    val in = new FileInputStream(tokFile)
    val out = new PrintStream(alignedFile)
    val err = new PrintStream(alignedLogFile)

    try {
      val aligner = new Aligner(
        in, out, err,
        useAligner3 = true,
        verbosity = 1,
        logUnalignedConcepts = true,
        printNodesAndEdges = true
      )
      aligner.run()
    } finally {
      in.close()
      out.close()
      err.close()
    }

    // ./cmd.aligned.no_opN
    logger.info(s"Removing :op ids (:opN -> :op) for $amrFileName")
    val alignedNoOpNFile = new File(s"${amrFile.getPath}.aligned.no_opN")
    Utils.using(new PrintWriter(alignedNoOpNFile)) { alignedNoOpNWriter =>
      Source.fromFile(alignedFile).getLines()
        .map(_.replaceAll(":op[^ ]*", ":op"))
        .foreach(alignedNoOpNWriter.println)
    }
  }

  // Analogue of cmd.aligned.concepts_no_opN
  def extractConceptsNoOpN(amrFileName: String): Unit = {
    logger.info(s"Extractig concepts (words span => concept) from aligment log for $amrFileName")

    val amrFile = new File(amrFileName)
    val alignedLogFile = new File(s"${amrFile.getPath}.aligned.log")
    val conceptsNoOpNFile = new File(s"${amrFile.getPath}.aligned.concepts_no_opN")

    val outWriter = new PrintWriter(conceptsNoOpNFile)

    try {
      val contextToEntries = Source.fromFile(alignedLogFile).getLines()
        .filter(_.startsWith("Span"))
        .map(span => span
          .replaceAll(":op[0-9]*", ":op")
          .replaceAll("^Span [0-9]*:  ", "")
          .replaceAll(" => ", " ||| "))
        .toList
        .groupBy(identity) // key -> list of lines equal to the key

      contextToEntries.toList
        .filter(!_._1.startsWith(" "))
        .sortBy(_._1) // sort by context name
        .map { case (k, v) => s"$k ||| Count=${v.size}" }
        .foreach(outWriter.println)
    } finally {
      outWriter.close()
    }
  }

  def runStandfordDependencyParser(inputFile: File, outputFile: String): Unit = {
    val in = new FileInputStream(inputFile)
    val out = new PrintStream(outputFile)
    val outKBest = new PrintStream(outputFile + ".k_best.txt")

    try {
      val parser = new RunStanfordParser(in, out, Some(outKBest), verbose = runProperties.getBool("verbose.preprocessing.dep"))
      parser.run()
    } finally {
      in.close()
      out.close()
    }
  }

  def runStandfordDependencyParser(amrFile: File): Unit = {
    logger.info(s"Run Standford Parser for ${amrFile.getName}")
    val sntFile = s"${amrFile.getPath}.snt"
    val depsFile = s"${amrFile.getPath}.snt.deps"
    runStandfordDependencyParser(new File(sntFile), depsFile)
  }

  // cmd.snt.IllinoisNER
  def runIllinoisNamedEntityTagger(amrFile: File): Unit = {
    logger.info(s"Run Named Entity Tagger for ${amrFile.getName}")
    val sntFile = s"$amrFile.snt"
    val nerFile = s"$amrFile.IllinoisNER"
    runIllinoisNamedEntityTagger(new File(sntFile), nerFile)
  }

  def runIllinoisNamedEntityTagger(inputFile: File,
                                   nerFile: String,
                                   outRedirect: PrintStream = System.out,
                                   errRedirect: PrintStream = System.err): Unit = {
    val nerPath = ctx.illinoisNerPath
    val cPathSeparator = Option(System.getProperty("path.separator")).getOrElse(";")
    val nerClasspath = Seq(nerPath, s"${nerPath}target/classes", s"${nerPath}target/dependency/*").mkString(cPathSeparator)

    val inputFileTmp = s"${inputFile.getPath}.tmp"
    val outputFile = nerFile
    val outputFileTmp = s"$outputFile.tmp"

    // cat "$inputfile" | sed $'s/$//\n####/\n/' > "$inputfile".tmp
    val tmpWriter = new PrintWriter(inputFileTmp)
    InputSentencesReader.getStream(Source.fromFile(inputFile))
      .map(_.sentence)
      .map(line => s"$line\n####\n")
      .foreach(tmpWriter.println)
    tmpWriter.close()

    val command =
      s"""java -cp "$nerClasspath"
         | -Xmx8g
         | edu.illinois.cs.cogcomp.LbjNer.LbjTagger.NerTagger
         | -annotate "$inputFileTmp"
         | "$outputFileTmp"
         | "${ctx.nerConfigPath}"
      """.stripMargin.replaceAll("/n", " ")
    val proc = Runtime.getRuntime.exec(command, null, new File(ctx.illinoisNerPath))
    StreamUtils.redirectStream(proc.getInputStream, outRedirect)
    StreamUtils.redirectStream(proc.getErrorStream, errRedirect)
    proc.waitFor()

    // TODO: This code is very poor (including IllinoisNERConvert), refactor it
    val baos = new ByteArrayOutputStream()
    val converter = new IllinoisNERConvert(null, out = new PrintStream(baos), System.err)
    Source.fromFile(outputFileTmp)
      .getLines()
      .flatMap(_.replaceAll(" #### ", "\n").split("\n"))
      .foreach(converter.convertLine)

    var output = new String(baos.toByteArray, StandardCharsets.UTF_8)
    output = output.substring(0, output.lastIndexOf("\n"))

    val writer = new PrintWriter(outputFile)
    writer.print(output)
    writer.close()
  }

}

object Preprocessor {
  // just local playground, nothing more...
  def main(args: Array[String]): Unit = {
    val jamrRoot = "C:/Users/unkarjedy/Desktop/Diploma/Jamr_Fork"

    val preprocessor = new Preprocessor(ContextBuilder.createContext(jamrRoot))
    val goldAmrs = new File(jamrRoot + "/resources_terms/FinalOutputs/gold_amr/sentences2.txt.amr_gold")

    preprocessor.extractSentencesAndTokens(goldAmrs)
    preprocessor.runAligner(goldAmrs)
  }
}