package edu.cmu.lti.nlp.amr

import java.io.{InputStream, PrintStream}

object IllinoisNERConvert {
  def main(args: Array[String]): Unit = {
    val runner = IllinoisNERConvert(System.in, System.out, System.err)
    runner.run()
  }
}

case class IllinoisNERConvert(in: InputStream,
                              out: PrintStream,
                              err: PrintStream) extends Runnable {
  override def run(): Unit = {
    Source.fromInputStream(in)
      .getLines()
      .foreach(line => {
        convertLine(line)
      })
  }

  def convertLine(line: String): Unit = {
    var tag = "O"
    var begin = false

    // more than one space, or an empty line, is skipped
    for (word <- line.split(" ") if word.trim != "") {
      if (word.matches("""\[[A-Z]+""")) {
        if (tag != "O") {
          err.println(" *** WARNING *** : Nested tags in Illinois NER output.\nLine = " + line + "\nSuggestion: you may want to remove any [ or ] from the input.")
        }
        tag = word.tail
        begin = true
      } else if (!word.endsWith("]") || word.length == 1) {
        if (tag != "O") {
          out.println(s"$word\t${if (begin) "B-" else "I-"}$tag")
        } else {
          out.println(s"$word\t$tag")
        }
        begin = false
      } else {
        if (tag == "O") {
          err.println(" *** WARNING *** : Closing bracket outside of tag in Illinois NER output.\nLine = " + line + "\nSuggestion: you may want to remove any [ or ] from the input.")
          out.println(s"${word.slice(0, word.length - 1)}\tO")
        } else {
          out.println(s"${word.slice(0, word.length - 1)}\t${if (begin) "B-" else "I-"}$tag")
        }
        tag = "O"
      }
    }
    out.print("\n")
  }
}



