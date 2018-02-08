package scripts

import scripts.train.RunProperties
import scripts.utils.context.{Context, ContextBuilder}


object WordnetStdinReader {

  val runProperties = new RunProperties()

  def main(args: Array[String]): Unit = {
    val context = ContextBuilder.createContext(runProperties)
  }

}
