package edu.cmu.lti.nlp.amr.utils

import java.io.File

import org.apache.commons.lang3.StringUtils

import scala.io.Source

object CorpusUtils {

  def splitFileOnNewline(file: String): Iterator[String] =
    splitOnNewline(Source.fromFile(file))

  def splitFileOnNewline(file: File): Iterator[String] =
    splitOnNewline(Source.fromFile(file))

  def splitOnNewline(source: Source): Iterator[String] =
    splitOnNewline(source.getLines())

  // This treats more than one newline in a row as a single newline
  def splitOnNewline(iterator: Iterator[String]): Iterator[String] = {
    (for {
      x <- iterator if x != ""
      p = (x :: iterator.takeWhile(StringUtils.isNotBlank).toList).mkString("\n")
    } yield p).filter(StringUtils.isNotBlank)
  }

  /**
    * Takes an iterator of lines, splits on empty lines, and yields only
    * blocks of lines that contain some AMR content
    */
  def getAMRBlocks(iterator: Iterator[String]): Iterator[String] = for (
    block <- splitOnNewline(iterator)
    if block.split("\n").exists(_.startsWith("(")) // needs to contain some AMR
  ) yield block

  def getDepsBlocks(iterator: Iterator[String]): Iterator[DepsTextBlock] = {
    CorpusUtils.splitOnNewline(iterator).zipWithIndex
      .map { case (block, idx) =>
        val blockLines = block.split("\\n").toSeq
        val (metaLines, conllLines) = blockLines.partition(_.startsWith("#"))

        val sntOpt: Option[String] = {
          val reg = "# ::snt\\s+(.+)".r
          metaLines.collectFirst { case reg(snt) => snt.trim }
        }
        val sntIdOpt: Option[Int] = {
          val reg = "# ::sntId\\s+(.+)".r
          metaLines.collectFirst { case reg(sntId) => sntId.trim.toInt }
        }
        val treeIdOpt: Option[String] = {
          val reg = "# ::treeId\\s+(.+)".r
          metaLines.collectFirst { case reg(treeId) => treeId.trim }
        }
        DepsTextBlock(conllLines.map(LineUtils.cleanDependencyStr), idx, sntOpt, sntIdOpt, treeIdOpt)
      }
  }
}

case class DepsTextBlock(conllLines: Seq[String], blockIdx: Int,
                         snt: Option[String],
                         sntId: Option[Int],
                         treeId: Option[String]) {
  def conllText = conllLines.mkString("\n")
}

class CorpusUtilsTest /* extends Suite*/ {
  def testSplitOnNewline() {
    val split = CorpusUtils.splitOnNewline(Iterator("a", "b", "c", "", "a", "c", "b"))
    assert(split.toList == List("a\nb\nc", "a\nc\nb"))
  }
}

