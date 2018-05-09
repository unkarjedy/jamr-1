package scripts.parse

import org.apache.commons.lang3.StringUtils

import scala.io.Source

object InputSentencesReader {
  object Format extends Enumeration {
    type Format = Value
    val Snt = Value("snt")
    val TermSnt = Value("term_snt")
  }

  def getStream(source: Source): Stream[SentenceWithTerm] = {
    val lines: Iterator[String] = source.getLines()
    val nonBlankLinesStream = lines.toStream.filter(StringUtils.isNotBlank)

    val formatReg = """^\s*#\s*format:?\s*([\w_]+)\s*$""".r
    nonBlankLinesStream.headOption match {
      case None =>
        Stream.empty
      case Some(head) if !head.trim.startsWith("#")=>
        nonBlankLinesStream.map(SentenceWithTerm(_, None))
      case Some(formatReg(format)) =>
        Format.withName(format) match {
          case Format.Snt =>
            nonBlankLinesStream.tail.map(SentenceWithTerm(_, None))
          case Format.TermSnt =>
            nonBlankLinesStream.tail.grouped(2).toStream.map(_.toList match {
              case t :: s :: Nil => SentenceWithTerm(s, Some(t))
              case t :: Nil => SentenceWithTerm("", Some(t))
            })
        }
      case Some(head) =>
        throw new RuntimeException(s"Unknown first line format: $head")
    }
  }

  case class SentenceWithTerm(sentence: String, term: Option[String] = None)

  def main(args: Array[String]): Unit = {
    test()
  }

  private def test(): Unit = {
    getStream(Source.fromString("")) shouldBe Seq()
    getStream(Source.fromString("one")) shouldBe Seq(SentenceWithTerm("one"))
    getStream(Source.fromString("one\n")) shouldBe Seq(SentenceWithTerm("one"))
    getStream(Source.fromString("\n\none\n")) shouldBe Seq(SentenceWithTerm("one"))
    getStream(Source.fromString("one\n\ntwo\nthree")) shouldBe Seq(
      SentenceWithTerm("one"),
      SentenceWithTerm("two"),
      SentenceWithTerm("three")
    )

    getStream(Source.fromString("# format: snt\none\n\ntwo\nthree")) shouldBe Seq(
      SentenceWithTerm("one"),
      SentenceWithTerm("two"),
      SentenceWithTerm("three")
    )

    getStream(Source.fromString("\n# format: term_snt\nterm1\n\nsent1\nterm2\nsent2\nterm3")).toArray.toSeq shouldBe Seq(
    )

    getStream(Source.fromString("\n# format: term_snt\nterm1\n\nsent1\nterm2\nsent2\nterm3")).toArray.toSeq shouldBe Seq(
      SentenceWithTerm("sent1", Some("term1")),
      SentenceWithTerm("sent2", Some("term2")),
      SentenceWithTerm("", Some("term3"))
    )

    println("All tests passed!")
  }

  private implicit class ScalaTest(val target: Any) extends AnyVal {
    def shouldBe(other: Any): Unit = {
      if (target != other) {
        throw new AssertionError(s"Assertion failed: $target is not equal to $other")
      }
    }
  }
}