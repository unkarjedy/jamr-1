package scripts.utils

import java.io.{FileInputStream, PrintStream}

import edu.cmu.lti.nlp.amr.AMRParser

object AMRParserRunner {

  private val stdIn = System.in
  private val stdOut = System.out
  private val stdErr = System.err

  def run(argsString: String,
          inFilePath: String,
          outFilePath: String,
          errFilePath: String): Unit = {
    val in = new FileInputStream(inFilePath)
    val out = new PrintStream(outFilePath)
    val err = new PrintStream(errFilePath)

    try {
      System.setIn(in)
      System.setOut(out)
      System.setErr(err)

      val args = ArgsParser.getArgsFromString(argsString)
      AMRParser.main(args)
    } finally {
      in.close()
      out.close()
      err.close()

      System.setIn(stdIn)
      System.setOut(stdOut)
      System.setErr(stdErr)
    }
  }

}
