package scripts

import java.io._

import edu.cmu.lti.nlp.amr.utils.{CorpusUtils, DepsTextBlock}
import org.apache.batik.transcoder.image.PNGTranscoder
import org.apache.batik.transcoder.{TranscoderInput, TranscoderOutput}
import org.apache.commons.io.FileUtils
import pio.gitlab.nats.deptreeviz.{DepTree, SimpleParse, SimpleWord}
import scripts.train.RunProperties
import scripts.utils.FileExt._
import scripts.utils.logger.SimpleLoggerLike

import scala.collection.JavaConversions._
import scala.io.Source
import scala.util.Try

class DependencySvgPngDrawer() extends SimpleLoggerLike {

  /**
    * Converts CONLL extracted rom dependenciesFileName to outputFolderName
    *
    * @param dependenciesFile file with colnn entries
    * @param outputFolder     resulting folder which will contain dependency trees render
    * @return true if drawing finished without any error
    */
  def draw(dependenciesFile: File, outputFolder: File, continue: Boolean = false): Unit = {
    outputFolder.mkdirs()

    val startFromBlockIdx = {
      lazy val files = outputFolder.listFiles()
      if (continue && files.nonEmpty) { // in order that .max works properly
        val regex = """(\d+)-s(\w+)\-.*""".r
        val (lastFileName, blockIdx, _) = files.map { file =>
          val fileName = file.getName
          val regex(fileIdx, blokcIdx) = fileName
          (fileName, fileIdx.toInt, blokcIdx)
        } maxBy(_._2)

        logger.info(s"Last rendered file: $lastFileName")
        Some(blockIdx)
      } else {
        None
      }
    }

    val lines = Source.fromFile(dependenciesFile).getLines()
    val totalBlocks = CorpusUtils.splitOnNewline(Source.fromFile(dependenciesFile)).size
    val depBlocks: Iterator[DepsTextBlock] = CorpusUtils.getDepsBlocks(lines)

    def renderConllBlock(block: DepsTextBlock) = {
      val fileName = buildBaseFileNameForBlock(block) + ".png"
      val outFile = outputFolder.resolve(fileName)
      val svgString = generateSvgToString(block)
      logger.info(s"CONLL -> SVG -> PNG ${block.blockIdx + 1} / $totalBlocks, fileName: $fileName")
      try {
        svg2png(svgString, outFile)
      } catch {
        case ex: Throwable =>
          outFile.delete()
          throw ex
      }
    }

    val depBlocksFinal = startFromBlockIdx match {
      case Some(idx) => depBlocks.dropWhile(_.blockIdx != idx).drop(1)
      case _ => depBlocks
    }
    depBlocksFinal.foreach { block =>
      renderConllBlock(block)
    }
  }

  private def buildBaseFileNameForBlock(block: DepsTextBlock): String = {
    val idx = block.blockIdx
    val sntIdOpt = block.sntId
    val treeIdOpt = block.treeId

    Seq(
      Some(idx.toString),
      sntIdOpt.map("s" + _),
      treeIdOpt.map("t" + _)
    ).flatten.mkString("-")
  }

  private def generateSvgToWriter(block: DepsTextBlock, writer: Writer): Unit = {
    try {
      val parser = SimpleParse.fromConll(block.conllLines)
      val depTree = new DepTree[SimpleParse, SimpleWord](parser)
      depTree.writeTree(writer)
    } catch {
      case ex: Exception =>
        logger.warning(s"[ERROR] Could not render to SVG block `${block.blockIdx}`")
        throw ex
    }
  }

  private def generateSvgToString(block: DepsTextBlock): String = {
    val writer = new StringWriter()
    generateSvgToWriter(block, writer)
    writer.toString
  }

  private def svg2png(svg: String, outFile: File): Unit = {
    val pngOutputStream = new FileOutputStream(outFile)
    svg2png(new ByteArrayInputStream(svg.getBytes()), pngOutputStream)
    pngOutputStream.close()
  }

  private def svg2png(inputStream: InputStream, outputStream: OutputStream): Unit = {
    val inputImage = new TranscoderInput(inputStream)
    val outputImage = new TranscoderOutput(outputStream)

    val converter = new PNGTranscoder
    converter.transcode(inputImage, outputImage)

    outputStream.flush()
  }
}

object DependencySvgPngDrawer extends SimpleLoggerLike {

  def main(args: Array[String]): Unit = {
    // usecase1()
    // usecase2()
    usecase3()
  }

  private def usecase3(): Unit = {
    val runProperties = new RunProperties("run.properties")
    val baseDir: File = new File(runProperties.jamrRoot)
      .resolve("resources_terms/FinalOutputs2/PART1/DEP_TREES")
    val files = baseDir.listFiles().filter(_.isFile)
    val outBaseDir = baseDir.resolve("images")

    logger.info(s"Render dependency tree files:")
    files.foreach(f => logger.info(s"    ${f.getName}"))
    logger.info(s"Render output base folder: $outBaseDir")

    files.foreach { file =>
      logger.info(s"Render dep trees for file: ${file.getName}")
      try {
        new DependencySvgPngDrawer().draw(
          dependenciesFile = file,
          outputFolder = outBaseDir.resolve(file.getName),
          continue = true
        )
      } catch {
        case ex: Throwable =>
          logger.warning("Some exception occurred during rendering")
          ex.printStackTrace()
      }
    }
  }

  private def usecase2(): Unit = {
    val runProperties = new RunProperties("run.properties")
    val baseDir: File = new File(runProperties.jamrRoot)
      .resolve("resources_terms/FinalOutputs/for_presentation")

    new DependencySvgPngDrawer().draw(
      dependenciesFile = baseDir.resolve("test1.conll"),
      outputFolder = baseDir
    )
  }

  private def usecase1(): Unit = {
    val runProperties = new RunProperties("run.properties")
    val baseFolder = new File(s"${runProperties.jamrRoot}/${runProperties.parserInputFolder}/")
    val depsFile = baseFolder.resolve("sentences2.txt.deps")
    val outDir = baseFolder.resolve("out").resolve("images")

    val renderer = new DependencySvgPngDrawer()

    val success = Try(renderer.draw(depsFile, outDir)).toOption.isDefined

    if (success) {
      val svgFolder = outDir.resolve("svg")
      svgFolder.mkdir()
      outDir.listFiles().filter(_.getName.endsWith(".svg")).foreach { f =>
        FileUtils.moveFile(f, svgFolder.resolve(f.getName))
      }
    }
  }
}