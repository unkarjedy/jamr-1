package term_dict_process

object MyStringUtils {

  def removeNonAsciiSymbols(string: String): String = {
    string.replaceAll("[^\\x20-\\x7e]", " ")
  }

}
