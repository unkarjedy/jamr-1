package term_dict

object MyStringUtils {

  def removeNonAsciiSymbols(string: String): String = {
    string.replaceAll("[^\\x20-\\x7e]", " ")
  }

}
