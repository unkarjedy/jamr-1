package scripts.train

import java.io.{BufferedInputStream, File, FileInputStream, FileNotFoundException}
import java.util.{Objects, Properties}

import scripts.utils.FileExt._

import scala.annotation.tailrec

final class RunProperties(filePath: String = "run.properties") extends Properties {

  private val resourcesFile = {
    val classesRoot = new File(this.getClass.getClassLoader.getResource(".").toURI)
    val candidate1 = classesRoot.resolve(filePath)
    if (candidate1.exists()) {
      candidate1
    } else {
      // Danger! Shit code... if resource file was not found in classpath then read it from... SRC FOLDER OMG!
      @tailrec
      def findSrcFolder(file: File): Option[File] = {
        file.listFiles.filter(_.getName == "src").toList match {
          case src :: Nil => Some(src)
          case _ if Objects.nonNull(file.getParentFile) => findSrcFolder(file.getParentFile)
          case _ => None
        }
      }

      val candidate2 = findSrcFolder(classesRoot).flatMap(s => Option(s.resolve(filePath)).filter(_.exists()))
      candidate2.getOrElse(throw new FileNotFoundException(filePath))
    }
  }
  System.out.println(s"Properties file: $resourcesFile")
  this.load(new BufferedInputStream(new FileInputStream(resourcesFile)))

  def jamrRoot = getProperty("jamrRoot")
  def corpusFolder = getProperty("corpus.folder")
  def corpusFileBaseName = getProperty("corpus.file_base_name")
  def modelFolder = getProperty("parse.model.folder")
  def modelFolderSuffix = getProperty("model.folder_suffix")

  def skipPreprocessing = getBool("skip.preprocessing")
  def skipTokenizer = getBool("skip.preprocessing.tokenizer")
  def skipNER = getBool("skip.preprocessing.ner")
  def skipDEP = getBool("skip.preprocessing.dep")
  def skipParse = getBool("skip.parse")
  def skipConceptTableExtraction = getBool("skip.concept_table_extract")
  def skipTrainStage1 = getBool("skip.train.stage1")
  def skipTrainStage2 = getBool("skip.train.stage2")
  def skipEvaluating = getBool("skip.evaluate")
  def skipEvaluateAllStageDecode = getBool("skip.evaluate.allstage.decode")
  def skipEvaluateStage2Decode = getBool("skip.evaluate.stage2.decode")
  def skipSvgRender = getBool("skip.svg_render", default = true)

  def parserInputFileName = getProperty("parser.input.file_name")
  def parserInputFolder = getProperty("parser.input.folder")
  def parserOutputFolder = getProperty("parser.output.folder")

  def getBool(prop: String, default: Boolean = false) = getProperty(prop, default.toString).toBoolean
}
