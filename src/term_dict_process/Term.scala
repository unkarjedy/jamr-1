package term_dict_process

import scala.collection.mutable

case class Term(id: Int, value: String,
                definitions: mutable.ArrayBuffer[TermDefinition] = mutable.ArrayBuffer(),
                synonyms: mutable.ArrayBuffer[String] = mutable.ArrayBuffer())
