package scripts.parse

import java.io.File

import org.apache.commons.io.FileUtils
import scripts.DependencySvgPngDrawer
import scripts.train.RunProperties
import scripts.utils.FileExt._
import scripts.utils.StageRunnerLike
import scripts.utils.context.{Context, ContextBuilder}

object RunParse extends StageRunnerLike {
  private val runProperties = new RunProperties("run.properties")

  private val jamrRoot = runProperties.jamrRoot

  private val stage1Features = List(
    "bias", "length", "firstMatch", "numberIndicator", "badConcept",
    "sentenceMatch", "andList", "pos", "posEvent",
    "phrase", "phraseConceptPair", "phraseConceptPairPOS",
    "corpusIndicator", "corpusLength", "count", "conceptGivenPhrase", "phraseGivenConcept"
  ).distinct

  private val stage2Features = List(
    "bias", "typeBias", "self", "fragHead", "edgeCount", "distance", "logDistance",
    "rootConcept", "rootDependencyPathv1",
    "conceptBigram",
    "posPathv3", "dependencyPathv4", "dependencyPathv5"
  ).distinct

  private val stage1SyntheticConcepts = List(
    "NER", "DateExpr", "OntoNotes", "verbs",
    // "NEPassThrough",
    "PassThrough",
    "WordNetPassThrough"
    // "TermsDict"
  ).distinct

  def main(args: Array[String]): Unit = {
    val modelFolder = s"$jamrRoot/models/${runProperties.modelFolder}"

    val context = {
      val c = ContextBuilder.createContext(jamrRoot, modelFolder)
      c.runProperties = runProperties
      c.stage1Weights = s"${c.modelFolder}/stage1-weights"
      c.stage2Weights = s"${c.modelFolder}/stage2-weights"
      // c.stage2Weights = s"${context.modelFolder}/stage2-weights.iter5"
      c.stage1Features = stage1Features
      c.stage2Features = stage2Features
      c.parserOptions = buildParserOptionsString(c, stage1SyntheticConcepts)
      c
    }

    val inputFilename = runProperties.parserInputFileName
    val inputFolder = s"$jamrRoot/${runProperties.parserInputFolder}"
    val outputFolder = s"$inputFolder/out"

    val parseStage = Parse(context, inputFolder, inputFilename, outputFolder)
    parseStage.run()

    runStage("### Extract AMRs only ###", skip = false) {
      val extractor = new ParseLogDataExtractor(outputFolder, inputFilename)
      extractor.extractAmrOnly()
    }

    runStage("### Render dependency trees ###", runProperties.skipSvgRender) {
      val baseFolder = new File(inputFolder)
      val dependenciesFile: File = baseFolder.resolve(s"$inputFilename.deps.k_best.txt")
      val outputDir: File = baseFolder.resolve("out").resolve("images")
      renderDependencyTrees(dependenciesFile, outputDir)
    }

    sys.exit(0) // some non-daemon threads are stuck... (probably from DependencySvgPngDrawer/Deptreeviz)
  }

  private def renderDependencyTrees(depFile: File, outDir: File) = {
    try {
      new DependencySvgPngDrawer().draw(depFile, outDir)
    } catch {
      case ex: Throwable =>
        ex.printStackTrace()
    }

    try {
      val svgFolder = outDir.resolve("svg")
      svgFolder.mkdir()
      outDir.listFiles().filter(_.getName.endsWith(".svg")).foreach { f =>
        val distFile = svgFolder.resolve(f.getName)
        distFile.delete()
        FileUtils.moveFile(f, distFile)
      }
    } catch {
      case ex: Throwable =>
        ex.printStackTrace()
    }
  }
  private def buildParserOptionsString(context: Context, stage1SyntheticConcepts: List[String]) = {
    s"""--stage1-synthetic-concepts ${stage1SyntheticConcepts.mkString(",")}
       |--stage1-phrase-counts ${context.modelFolder}/wordCounts.train
       |--stage1-features ${context.stage1Features.mkString(",")}
       |--stage2-features ${context.stage2Features.mkString(",")}
       |--stage2-decoder LR
       |--stage2-labelset ${context.jamrRoot}/resources/labelset-r3
       |--output-format AMR,nodes,edges,root
       |--ignore-parser-errors
       |--print-stack-trace-on-errors
       |--terms-dict "${context.jamrRoot}/resources_terms/terms_with_synonims.txt"
       |-v 1
      """.stripMargin
  }
}
