package scripts.utils

import java.io.{BufferedReader, InputStream, InputStreamReader, PrintStream}

object StreamUtils {

  def redirectStream(from: InputStream, to: PrintStream): Unit = {
    val in = new BufferedReader(new InputStreamReader(from))
    var line = in.readLine()
    while (line != null) {
      to.println(line)
      line = in.readLine()
    }
  }

}
