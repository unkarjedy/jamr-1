package scripts

import java.io.{File, FileInputStream, FileOutputStream, FileWriter}

import edu.cmu.lti.nlp.amr.utils.{CorpusUtils, DepsTextBlock}
import org.apache.batik.transcoder.image.PNGTranscoder
import org.apache.batik.transcoder.{TranscoderInput, TranscoderOutput}
import org.apache.commons.io.FileUtils
import org.apache.commons.lang3.StringUtils
import pio.gitlab.nats.deptreeviz.{DepTree, SimpleParse, SimpleWord}
import scripts.train.RunProperties
import scripts.utils.FileExt._
import scripts.utils.logger.SimpleLoggerLike

import scala.collection.JavaConversions._
import scala.collection.mutable
import scala.io.Source
import scala.util.{Failure, Try}

object DependencySvgPngDrawer extends SimpleLoggerLike {
  private val runProperties = new RunProperties()
  private val jamrRoot = runProperties.jamrRoot

  def main(args: Array[String]): Unit = {
    val baseFolder = new File(s"$jamrRoot/resources_terms/parse_RNN/")
    val imagesFolder = baseFolder.resolve("out").resolve("images")
    val dependenciesFile = baseFolder.resolve("sentences2.txt.deps")
    val success = draw(dependenciesFile, imagesFolder)

    if (success) {
      val svgFolder = imagesFolder.resolve("svg")
      svgFolder.mkdir()
      imagesFolder.listFiles().filter(_.getName.endsWith(".svg")).foreach { f =>
        FileUtils.moveFile(f, svgFolder.resolve(f.getName))
      }
    }
  }

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
    val totalBlocks = Source.fromFile(dependenciesFile).getLines().count(StringUtils.isBlank) + 1
    val seenConllHashes = mutable.Set[Int]()
    CorpusUtils.getDepsBlocks(lines)
      .foreach { case block@DepsTextBlock(conllLines, idx, sntOpt, sntIdOpt, treeIdOpt) =>
        val blockHash: Int = block.conllText.hashCode
        val fileName = Seq(
//          Some(idx.toString),
          sntIdOpt.map("s" + _),
          treeIdOpt.map("t" + _)
        ).flatten.mkString("-")
        if (seenConllHashes.contains(blockHash)) {
          logger.warning(s"Skipping file $fileName with duplicate conll block")
        } else {
          seenConllHashes += blockHash
          generateSvg(outputFolder, conllLines, fileName)
          logger.info(s"CONLL -> SVG ${idx + 1} / $totalBlocks")
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
    val parser = SimpleParse.fromConll(conll)
    val depTree = new DepTree[SimpleParse, SimpleWord](parser)

    val svgFilePath = s"$outputFolder/$outFileName.svg"
    val writer = new FileWriter(svgFilePath)
    depTree.writeTree(writer)
    writer.close()
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
