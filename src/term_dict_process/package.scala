import scala.collection.mutable

package object term_dict_process {

  type MArrayBuffer[T] = mutable.ArrayBuffer[T]

  case class Term(id: Int, value: String,
                  definitions: MArrayBuffer[TermDefinition] = mutable.ArrayBuffer(),
                  synonyms: MArrayBuffer[String] = mutable.ArrayBuffer())

  case class TermDefinition(id: Int, termId: Int,
                            value: String,
                            var sentences: MArrayBuffer[Sentence] = mutable.ArrayBuffer())

  case class TermWithSentences(term: String, sentences: Seq[Sentence])

  case class Sentence(value: String) {
    /** terms that the sentence value contains (lowecase-compared) */
    var termOccurrences: mutable.Map[String, Int] = mutable.Map().withDefaultValue(0)

    def updated(f: String => String): Sentence = copy(value = f(value))
  }

  case class TermUsages(usages: Int, term: String) {
    override def toString: String = s"$usages / $term"
  }


  case class TermUsagesMutable(var usages: Int, term: String) {
    def toImmutable: TermUsages = TermUsages(usages, term)
  }

}
