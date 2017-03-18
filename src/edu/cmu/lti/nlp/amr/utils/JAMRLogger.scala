package edu.cmu.lti.nlp.amr.utils

import java.io.PrintStream

// TODO: replace all logger function over the code with this logger
// TODO: GLOBAL VARIABLES IS AN EVIL!!!
case class JAMRLogger(out: PrintStream, verbosity: Int) {
  def log(level: Int, str: Any): Unit = {
    if (level <= verbosity) {
      out.println(str)
    }
  }
}