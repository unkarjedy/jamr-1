package scripts

import java.io.{File, FileInputStream, FileOutputStream, FileWriter}

import edu.cmu.lti.nlp.amr.utils.{CorpusUtils, DepsTextBlock}
import org.apache.batik.transcoder.image.PNGTranscoder
import org.apache.batik.transcoder.{TranscoderInput, TranscoderOutput}
import org.apache.commons.io.FileUtils
import pio.gitlab.nats.deptreeviz.{DepTree, SimpleParse, SimpleWord}
import scripts.DependencySvgPngDrawer.DrawerSettings
import scripts.train.RunProperties
import scripts.utils.FileExt._
import scripts.utils.logger.SimpleLoggerLike
import term_dict_process.Utils

import scala.collection.JavaConversions._
import scala.collection.mutable
import scala.io.Source
import scala.util.{Failure, Try}

class DependencySvgPngDrawer(settings: DrawerSettings = DrawerSettings()) extends SimpleLoggerLike {

  /**
    * Converts CONLL extracted rom dependenciesFileName to outputFolderName
    *
    * @param dependenciesFile file with colnn entries
    * @param outputFolder     resulting folder which will contain dependency trees render
    * @return true if drawing finished without any error
    */
  def draw(dependenciesFile: File, outputFolder: File): Boolean = {
    outputFolder.mkdirs()

    // generate SVGs
    val lines = Source.fromFile(dependenciesFile).getLines()
    val totalBlocks = CorpusUtils.splitOnNewline(Source.fromFile(dependenciesFile)).size
    val seenConllHashes = mutable.Set[Int]()

    val dependencyBlocks: Iterator[DepsTextBlock] = CorpusUtils.getDepsBlocks(lines)

    dependencyBlocks
      .foreach { block =>
        val conllLines = block.conllLines
        val idx = block.blockIdx
        val sntIdOpt = block.sntId
        val treeIdOpt = block.treeId

        val blockHash: Int = block.conllText.hashCode
        val fileName = Seq(
          Some(idx.toString),
          sntIdOpt.map("s" + _),
          treeIdOpt.map("t" + _)
        ).flatten.mkString("-")

        if (seenConllHashes.contains(blockHash)) {
          logger.warning(s"Skipping file $fileName with duplicate conll block")
        } else {
          seenConllHashes += blockHash
          generateSvg(outputFolder, conllLines, fileName)
          logger.info(s"CONLL -> SVG ${idx + 1} / $totalBlocks, fileName: $fileName")
        }
      }

    // convert all generated SVG to PNG image
    Try {
      val svgFiles = outputFolder.listFiles().filter(_.getName.endsWith(".svg"))
      svgFiles.zipWithIndex.foreach { case (svgFile, idx) =>
        logger.info(s"SVG -> PNG ${idx + 1} / ${svgFiles.size}")
        svg2png(svgFile)
      }
    } match {
      case Failure(t) =>
        t.printStackTrace(System.err)
        false
      case _ =>
        true
    }
  }

  private def generateSvg(outputFolder: File, conll: Seq[String], outFileName: String): Unit = {
    try {
      val parser = SimpleParse.fromConll(conll)
      val depTree = new DepTree[SimpleParse, SimpleWord](parser)
      val svgFilePath = s"$outputFolder/$outFileName.svg"
      Utils.using(new FileWriter(svgFilePath)) { writer =>
        depTree.writeTree(writer)
        writer.close()
      }
    } catch {
      case ex: Exception =>
        logger.info(s"[ERROR] Could not render file `$outFileName`")
        ex.printStackTrace()
    }
  }

  private def svg2png(file: File) = {
    val svgInputStream = new FileInputStream(file)
    val pngOutputStream = new FileOutputStream(file.getParentFile.resolve(file.getName.replaceAll("\\.svg$", "") + ".png"))

    val inputImage = new TranscoderInput(svgInputStream)
    val outputImage = new TranscoderOutput(pngOutputStream)

    val converter = new PNGTranscoder
    converter.transcode(inputImage, outputImage)

    pngOutputStream.flush()
    pngOutputStream.close()
  }
}

object DependencySvgPngDrawer extends SimpleLoggerLike {

  case class DrawerSettings()

  def main(args: Array[String]): Unit = {
    // usecase1()
    usecase2()
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

    val success = renderer.draw(depsFile, outDir)

    if (success) {
      val svgFolder = outDir.resolve("svg")
      svgFolder.mkdir()
      outDir.listFiles().filter(_.getName.endsWith(".svg")).foreach { f =>
        FileUtils.moveFile(f, svgFolder.resolve(f.getName))
      }
    }
  }
}