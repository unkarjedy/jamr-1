package term_dict

import org.apache.commons.lang3.StringUtils

object MyStringUtils {

  def removeNonAsciiSymbols(string: String): String = {
    string.replaceAll("[^\\x20-\\x7e]", " ")
  }

}
