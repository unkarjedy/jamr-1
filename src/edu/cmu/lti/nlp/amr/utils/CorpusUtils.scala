package edu.cmu.lti.nlp.amr.utils

import java.io.File

import org.apache.commons.lang3.StringUtils

import scala.io.Source

object CorpusUtils {

  def splitFileOnNewline(file: String): Iterator[String] =
    splitOnNewline(Source.fromFile(file))

  def splitFileOnNewline(file: File): Iterator[String] =
    splitOnNewline(file)

  def splitOnNewline(file: File): Iterator[String] =
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

  def getDepsBlocks(file: File): Iterator[DepsTextBlock] =
    getDepsBlocks(Source.fromFile(file))

  def getDepsBlocks(source: Source): Iterator[DepsTextBlock] =
    getDepsBlocks(source.getLines())

  def getDepsBlocks(iterator: Iterator[String]): Iterator[DepsTextBlock] = {
    CorpusUtils.splitOnNewline(iterator).zipWithIndex
      .map { case (block, idx) =>
        val blockLines = block.split("\\n").toSeq
        val (metaLines, conllLines) = blockLines.partition(_.trim.startsWith("#"))
        val reg = """#\s*\:\:(\w+)\s+(.+)""".r
        val tags = metaLines.map { case reg(tag, value) =>
          (tag, value.trim)
        }
        DepsTextBlock(idx, conllLines.map(LineUtils.cleanDependencyStr), tags)
      }
  }
}

case class DepsTextBlock(blockIdx: Int, conllLines: Seq[String],
                         tags: Seq[(String, String)]) {
  import DepsTextBlock._

  def snt: Option[String] = tags.find(_._1 == "snt").map(_._2)

  def sntId: Option[Int] = tags.find(_._1 == SntIdTag).map(_._2.toInt)

  def treeId: Option[String] = tags.find(_._1 == TreeIdTag).map(_._2)

  def withTag(tag: String, value: String) = copy(tags = tags :+ (tag, value))

  def withSntId(value: String) = copy(tags = tags :+ (SntIdTag, value))

  def withTreeId(value: String) = copy(tags = tags :+ (TreeIdTag, value))

  def conllText = conllLines.mkString("\n")

  def mkString = {
    val tagsBlock = tags map { case (tag, value) => s"# ::$tag $value" } mkString ("\n")
    tagsBlock + "\n" + conllText
  }
}

object DepsTextBlock {
  private val SntIdTag = "sntId"
  private val TreeIdTag = "treeId"
}

object CorpusUtilsTest /* extends Suite*/ {
  def main(args: Array[String]): Unit = {
    testSplitOnNewline()
    testGetDepsBlocks()
    println("All tests PASSED")
  }

  private def testSplitOnNewline() {
    val split = CorpusUtils.splitOnNewline(Iterator("a", "b", "c", "", "a", "c", "b"))
    assert(split.toList == List("a\nb\nc", "a\nc\nb"))
  }

  private def testGetDepsBlocks() {
    val text =
      """
        |
        | # ::snt raw facts , which are
        |# ::sntId   11
        |  # ::treeId   GOLD
        |1	raw	_	JJ	JJ	_	2	amod	_	_
        |2	facts	_	NNS	NNS	_	0	root	_	_
        |3	,	_	,	,	_	2	punct	_	_
        |4	which	_	WDT	WDT	_	6	nsubjpass	_	_
        |5	are	_	VBP	VBP	_	6	auxpass	_	_
        |
        |
      """.stripMargin
    val depBlocks = CorpusUtils.getDepsBlocks(text.lines).toArray.toSeq
    assertResult(depBlocks.size, 1)
    val block = depBlocks.head
    assertResult(block.blockIdx, 0)
    assertResult(block.sntId, Some(11))
    assertResult(block.treeId, Some("GOLD"))
    assertResult(block.withTag("newTag", "newTagValue").mkString,
      """# ::snt raw facts , which are
        |# ::sntId 11
        |# ::treeId GOLD
        |# ::newTag newTagValue
        |1	raw	_	JJ	JJ	_	2	amod	_	_
        |2	facts	_	NNS	NNS	_	0	root	_	_
        |3	,	_	,	,	_	2	punct	_	_
        |4	which	_	WDT	WDT	_	6	nsubjpass	_	_
        |5	are	_	VBP	VBP	_	6	auxpass	_	_""".stripMargin.replaceAll("\r", ""))
  }

  private def assertResult(result: Any, expected: Any) = {
    if (result == expected) {
      println("OK")
    } else {
      println("ERROR")
      println(s"EXPECTED  : $expected")
      println(s"RESULT    : $result")
      (expected, result) match {
        case (s1: String, s2: String) => println(s"DIFF: ${s1.diff(s2)}")
        case _ =>
      }
      throw new RuntimeException("Test failed")
    }
  }
}

