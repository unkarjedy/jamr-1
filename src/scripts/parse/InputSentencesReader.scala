package scripts.parse

import edu.cmu.lti.nlp.amr.utils.CorpusUtils
import org.apache.commons.lang3.StringUtils

import scala.io.Source
import scala.language.postfixOps

object InputSentencesReader {
  object Format extends Enumeration {
    type Format = Value
    val Snt = Value("snt")
    val TermSnt = Value("term_snt")
  }

  def getStream(source: Source): Stream[SentenceWithTerm] = {
    val lines: Iterator[String] = source.getLines()

    val linesStream = lines.toStream.dropWhile(StringUtils.isBlank)
      .filterNot(_.startsWith("# size"))
      .filterNot(_.startsWith("# pattern"))

    val formatReg = """^\s*#\s*format:?\s*([\w_]+)\s*$""".r
    linesStream.headOption match {
      case None =>
        Stream.empty
      case Some(head) if !head.trim.startsWith("#") =>
        extractSimple(linesStream)
      case Some(formatReg(format)) =>
        Format.withName(format) match {
          case Format.Snt => extractSimple(linesStream.tail)
          case Format.TermSnt => extractComplex(linesStream.tail)
        }
      case Some(head) =>
        throw new RuntimeException(s"Unknown first line format: $head")
    }
  }

  private def extractSimple(linesStream: Stream[String]): Stream[SentenceWithTerm] = {
    linesStream.filter(StringUtils.isNotBlank).zipWithIndex.map { case (el, idx) => SentenceWithTerm(idx, el, None) }
  }

  private def extractComplex(linesStream: Stream[String]): Stream[SentenceWithTerm] = {
    val blocks = CorpusUtils.splitOnNewline(linesStream.iterator).filter(StringUtils.isNotBlank)

    def parseGroup(block: String, idx: Int) = {
      val lines = block.lines.toList
      val sntIdReg = "#\\s*::\\s*sntId\\s*(.*)".r
      lines match {
        case sntIdReg(sntId) :: t :: s :: Nil => SentenceWithTerm(sntId.toInt, s, Some(t))
        case sntIdReg(sntId) :: t :: Nil => SentenceWithTerm(sntId.toInt, "", Some(t))
        case t :: s :: Nil => SentenceWithTerm(idx, s, Some(t))
        case t :: Nil => SentenceWithTerm(idx, "", Some(t))
        case list =>
          throw new RuntimeException(s"Wrong format of file with sentences: $list")
      }
    }

    blocks.zipWithIndex.map { case (group, idx) => parseGroup(group, idx) }.toStream
  }

  case class SentenceWithTerm(sntId: Int, sentence: String, term: Option[String] = None)

  def main(args: Array[String]): Unit = {
    test()
  }

  private def test(): Unit = {
    getStream(Source.fromString("")) shouldBe Seq()
    getStream(Source.fromString("one")) shouldBe Seq(SentenceWithTerm(0, "one"))
    getStream(Source.fromString("one\n")) shouldBe Seq(SentenceWithTerm(0, "one"))
    getStream(Source.fromString("\n\none\n")) shouldBe Seq(SentenceWithTerm(0, "one"))
    getStream(Source.fromString("one\n\ntwo\nthree")) shouldBe Seq(
      SentenceWithTerm(0, "one"),
      SentenceWithTerm(1, "two"),
      SentenceWithTerm(2, "three")
    )

    val terms = getStream(Source.fromString("# format: snt\none\n\ntwo\nthree")).toArray.toSeq
    terms shouldBe Seq(
      SentenceWithTerm(0, "one"),
      SentenceWithTerm(1, "two"),
      SentenceWithTerm(2, "three")
    )

    getStream(Source.fromString("\n# format: term_snt")).toArray.toSeq shouldBe Seq()

    getStream(Source.fromString(
      """
        |# format: term_snt
        |term1
        |sent1
        |
        |term2
        |sent2
        |
        |term3""".stripMargin)).toArray.toSeq shouldBe Seq(
      SentenceWithTerm(0, "sent1", Some("term1")),
      SentenceWithTerm(1, "sent2", Some("term2")),
      SentenceWithTerm(2, "", Some("term3"))
    )

    getStream(Source.fromString(
      """
        |# format: term_snt
        |# ::sntId 3
        |term1
        |sent1
        |
        |# ::sntId 5
        |term2
        |sent2
        |
        |
        |# ::sntId 9
        |term3""".stripMargin)).toArray.toSeq shouldBe Seq(
      SentenceWithTerm(3, "sent1", Some("term1")),
      SentenceWithTerm(5, "sent2", Some("term2")),
      SentenceWithTerm(9, "", Some("term3"))
    )

    getStream(Source.fromString(
      """
        |# format: term_snt
        |# size: 100500
        |# ::sntId 3
        |term1
        |sent1""".stripMargin)).toArray.toSeq shouldBe Seq(
      SentenceWithTerm(3, "sent1", Some("term1"))
    )

    getStream(Source.fromString(
      """
        |# format: term_snt
        |# size: 100500
        |# pattern qwe
        |# ::sntId 3
        |term1
        |sent1""".stripMargin)).toArray.toSeq shouldBe Seq(
      SentenceWithTerm(3, "sent1", Some("term1"))
    )

    getStream(Source.fromString(
      """
        |# format: term_snt
        |# size: 42
        |
        |# ::sntId 3
        |term1
        |sent1""".stripMargin)).toArray.toSeq shouldBe Seq(
      SentenceWithTerm(3, "sent1", Some("term1"))
    )

    println("All tests passed!")
  }

  private implicit class ScalaTest(val target: Any) extends AnyVal {
    def shouldBe(other: Any): Unit = {
      if (target != other) {
        throw new AssertionError(s"Assertion failed: \n$target \nis not equal to \n$other")
      }
    }
  }
}