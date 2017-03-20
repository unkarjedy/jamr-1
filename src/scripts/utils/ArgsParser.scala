package scripts.utils

import java.util
import java.util.regex.Pattern

/**
  * Parses input string to arguments array
  * a1 --a2 "a4 a5" -> ("a1, "--a2", "a4 a5")
  * http://stackoverflow.com/questions/7804335/split-string-on-spaces-in-java-except-if-between-quotes-i-e-treat-hello-wor
  */
object ArgsParser {

  def getArgsFromString(str: String): Array[String] = {
    val oneLineStr = str.replaceAll("\\n", " ")
    val matcher = Pattern.compile("([^\"]\\S*|\".+?\")\\s*").matcher(oneLineStr)

    val list = new util.ArrayList[String]()
    while (matcher.find()) {
      list.add(matcher.group(1))
    }

    list.toArray(Array[String]()).map(_.replaceAll("\"", ""))
  }

}
