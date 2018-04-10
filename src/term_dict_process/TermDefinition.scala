package term_dict_process

import scala.collection.mutable

case class TermDefinition(id: Int, termId: Int,
                          value: String,
                          var sentences: mutable.ArrayBuffer[String] = mutable.ArrayBuffer())
