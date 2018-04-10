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
    //    "phrase", "phraseConceptPair", "phraseConceptPairPOS",
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
    //    "NEPassThrough",
    "PassThrough",
    "WordNetPassThrough",
    "TermsDict"
  ).distinct

  def main(args: Array[String]): Unit = {
    val modelFolder = s"$jamrRoot/models/${runProperties.modelFolder}"

    val context = ContextBuilder.createContext(jamrRoot, modelFolder)
    context.runProperties = runProperties
    context.stage1Weights = s"${context.modelFolder}/stage1-weights"
    context.stage2Weights = s"${context.modelFolder}/stage2-weights"
    // context.stage2Weights = s"${context.modelFolder}/stage2-weights.iter5"
    context.stage1Features = stage1Features
    context.stage2Features = stage2Features
    context.parserOptions = buildParserOptionsString(context, stage1SyntheticConcepts)

    val inputFilename = runProperties.parserInputFileName
    val inputFolder = s"$jamrRoot/${runProperties.parserInputFolder}"
    val outputFolder = s"$inputFolder/out"

    val parseStage = Parse(context, inputFolder, inputFilename, outputFolder)
    parseStage.run()

    val extractor = new ParseLogDataExtractor(outputFolder, inputFilename)
    extractor.extractAmrOnly()

    runStage("### Draw SVG for dependency trees ###", skip = runProperties.skipSvgRender) {
//      val depsFileName = s"$inputFilename.deps"
      val depsFileName = s"$inputFilename.deps.k_best.txt"
      val baseFolder = new File(inputFolder)
      val imagesFolder = baseFolder.resolve("out").resolve("images")
      DependencySvgPngDrawer.draw(baseFolder.resolve(depsFileName), imagesFolder)

      val svgFolder = imagesFolder.resolve("svg")
      svgFolder.mkdir()
      imagesFolder.listFiles().filter(_.getName.endsWith(".svg")).foreach { f =>
        val distFile = svgFolder.resolve(f.getName)
        distFile.delete()
        FileUtils.moveFile(f, distFile)
      }
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
