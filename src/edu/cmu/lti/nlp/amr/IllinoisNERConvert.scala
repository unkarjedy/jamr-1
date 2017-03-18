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
    var tag: String = "O"
    var begin = false
    for (word <- line.split(" ") if word != "") {
      // more than one space, or an empty line, is skipped
      if (word.matches("""\[[A-Z]+""")) {
        if (tag != "O") {
          err.println(" *** WARNING *** : Nested tags in Illinois NER output.\nLine = " + line + "\nSuggestion: you may want to remove any [ or ] from the input.")
        }
        tag = word.tail
        begin = true
      } else if (!word.endsWith("]") || word.length == 1) {
        if (tag != "O") {
          out.println(word + "\t" + label(begin) + tag)
        } else {
          out.println(word + "\t" + tag)
        }
        begin = false
      } else {
        if (tag == "O") {
          err.println(" *** WARNING *** : Closing bracket outside of tag in Illinois NER output.\nLine = " + line + "\nSuggestion: you may want to remove any [ or ] from the input.")
          out.println(word.slice(0, word.length - 1) + "\tO")
        } else {
          out.println(word.slice(0, word.length - 1) + "\t" + label(begin) + tag)
        }
        tag = "O"
      }
    }
    out.print("\n")
  }
  private def label(begin: Boolean) = {
    if (begin) "B-"
    else "I-"
  }
}



