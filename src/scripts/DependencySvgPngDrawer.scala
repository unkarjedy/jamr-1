package scripts

import java.io.{File, FileInputStream, FileOutputStream, FileWriter}

import pio.gitlab.nats.deptreeviz.{DepTree, SimpleParse, SimpleWord}
import org.apache.batik.transcoder.image.PNGTranscoder
import org.apache.batik.transcoder.{TranscoderInput, TranscoderOutput}
import org.apache.commons.io.FileUtils
import scripts.train.RunProperties
import scripts.utils.FileExt._
import scripts.utils.logger.SimpleLoggerLike

import scala.collection.JavaConversions._
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

    if(success){
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
    *                         @return true if drawing finished without any error
    */
  def draw(dependenciesFile: File, outputFolder: File): Boolean = {
    outputFolder.mkdirs()

    val text = Source.fromFile(dependenciesFile).getLines().mkString("\n")
    val conllGroups = text.split("\\n\\n+").map(_.split("\\n").toList)
    conllGroups.zipWithIndex.foreach { case (conll, id) =>
      generateSvg(outputFolder, conll, id)
      logger.info(s"CONLL -> SVG ${id + 1} / ${conllGroups.length}")
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

  private def generateSvg(outputFolder: File, conll: List[String], id: Int): Unit = {
    val parser = SimpleParse.fromConll(conll)
    val depTree = new DepTree[SimpleParse, SimpleWord](parser)

    val svgFilePath = s"$outputFolder/$id.svg"
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
