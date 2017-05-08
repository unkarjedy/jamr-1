package edu.cmu.lti.nlp.amr.ConceptInvoke

import edu.cmu.lti.nlp.amr.BasicFeatureVector._
import edu.cmu.lti.nlp.amr._
import edu.cmu.lti.nlp.amr.term.{Term, TermsDict}

import scala.collection.mutable.ArrayBuffer
import scala.collection.{mutable => m}

object ConceptInvoker {
  val implementedFeatures = m.Set("fromNERTagger", "dateExpression") // TODO: check
}

/**
  * This class contains the code used to invoke concepts.
  * ConceptInvoker are invoked by calling the invoke() method, which returns a list of all
  * the concepts that match a span starting at index wordId of the tokenized sentence.
  */
class ConceptInvoker(options: m.Map[Symbol, String],
                     phraseConceptPairs: Array[PhraseConceptPair]) {
  // maps the first word in the phrase to a list of phraseConceptPairs
  private val conceptTable = m.Map[String, List[PhraseConceptPair]]()

  for (pair <- phraseConceptPairs) {
    val word = pair.words.head
    conceptTable(word) = pair :: conceptTable.getOrElse(word, List())
  }

  private val termsDict = new TermsDict(options.get('termsDict))


  /** ***** Concept sources to add *********
    *- Nominalizations
    *- List of -er => person ARG0-of things
    *- Entities from large list
    * ****************************************/
  private val conceptSources = options.getOrElse('stage1SyntheticConcepts, "NER,DateExpr").split(",").toSet
  private val implementedConceptSources = m.Set(
    "NER", "DateExpr", "OntoNotes", "verbs", "nominalizations",
    "NEPassThrough", "PassThrough", "WordNetPassThrough",
    "TermsDict"
  )

  private val unknownConcepts = conceptSources.diff(implementedConceptSources)
  assert(unknownConcepts.isEmpty, "Unknown conceptSources: " + unknownConcepts.mkString(", "))

  private var tokens = Array[String]() // stores sentence.drop(wordId) (used in the dateEntity code to make it more concise)
  private var lemmas = m.Set[String]() // TODO: check for lemma in a large morph-analyzed corpus

  // set of Propbank predicates (e.g. yield-03, baptize-02) (actually seams like contains not only predicates...)
  private var ontoNotes = m.Set[String]() // could be multi-map instead

  if (options.contains('stage1Predicates)) {
    val PredicateRegexp = """(.+)-([0-9]+)""".r
    for (predicate <- Source.fromFile(options('stage1Predicates)).getLines) {
      val PredicateRegexp(verb, senseId) = predicate
      ontoNotes += verb
    }
  }

  private val optionsLeaveOneOut = options.contains('stage1TrainingLeaveOneOut)
  private val optionsNer = options.contains('ner)
  private val optionsStage1Wiki = options.contains('stage1Wiki)

  /**
    * returns a list of all concepts that can be invoke starting at
    * position wordId in input.sentence (wordId.e. position wordId in the tokenized input)
    * Note: none of the concepts returned have spans that go past the end of the sentence
    */
  def invoke(input: Input, wordId: Int, trainingIndex: Option[Int]): List[PhraseConceptPair] = {
    val sentence = input.sentence
    if (wordId < 0 || wordId >= sentence.length) {
      return List()
    }

    var conceptList = getPossibleConceptsFromTable(sentence, wordId, trainingIndex)

    if (conceptSources.contains("NER") && optionsNer) {
      conceptList = input.ner.annotation
        .filter(_.start == wordId)
        .map(x => namedEntity(input, x))
        .toList ::: conceptList
    }
    if (conceptSources.contains("DateExpr")) {
      conceptList = dateEntities(input, wordId) ::: conceptList
    }

    // value indicates the only the pass through rules apply for this span
    val onlyPassThrough = conceptList.isEmpty
    if (conceptSources.contains("OntoNotes")) {
      conceptList = ontoNotesLookup(input, wordId, onlyPassThrough) ::: conceptList
    }
    if (conceptSources.contains("NEPassThrough")) {
      conceptList = NEPassThrough(input, wordId, onlyPassThrough) ::: conceptList
    }
    if (conceptSources.contains("PassThrough")) {
      conceptList = passThrough(input, wordId, onlyPassThrough) ::: conceptList
    }
    if (conceptSources.contains("WordNetPassThrough")) {
      conceptList = wordnetPassThrough(input, wordId, onlyPassThrough) ::: conceptList
    }
    if (conceptSources.contains("verbs")) {
      conceptList = verbs(input, wordId, onlyPassThrough) ::: conceptList
    }
    if (conceptSources.contains("nominalizations")) {
      conceptList = nominalizations(input, wordId, onlyPassThrough) ::: conceptList
    }
    if (conceptSources.contains("TermsDict")) {
      conceptList = termsLookup(input, wordId, onlyPassThrough) ::: conceptList
    }

    // Normalize the concept list by mergin features and trainIndexes of duplicating concept pairs
    val conceptSet: m.Map[(List[String], String), PhraseConceptPair] = m.Map()
    for (concept <- conceptList.filter(conceptPair => equalToSentenceWords(conceptPair, sentence, wordId))) {
      // TODO: make this case insensitive?
      val key = (concept.words, concept.graphFrag)
      if (conceptSet.contains(key)) {
        conceptSet(key) = mergeConcepts(concept, conceptSet(key))
      } else {
        conceptSet(key) = concept
      }
    }

    conceptSet.values.toList
  }

  private def mergeConcepts(first: PhraseConceptPair, second: PhraseConceptPair): PhraseConceptPair = {
    val fetures = FeatureVectorBasic()
    fetures += first.features
    fetures += second.features
    val trainingIndices = first.trainingIndices ::: second.trainingIndices
    PhraseConceptPair(first.words, first.graphFrag, fetures, trainingIndices)
  }

  // TODO: is this case insensitive??
  def equalToSentenceWords(conceptPair: PhraseConceptPair, sentence: Array[String], wordId: Int) = {
    conceptPair.words == sentence.slice(wordId, wordId + conceptPair.words.size).toList
  }

  private def getPossibleConceptsFromTable(sentence: Array[String], wordId: Int, trainingIndex: Option[Int]): List[PhraseConceptPair] = {
    val concepList0: List[PhraseConceptPair] = conceptTable.getOrElse(sentence(wordId), List())
    if (optionsLeaveOneOut && trainingIndex.isDefined) {
      concepList0.filter(conceptPair => {
        equalToSentenceWords(conceptPair, sentence, wordId) && conceptPair.trainingIndices.exists(id => abs(id - trainingIndex.get) > 20)
      })
    } else {
      concepList0.filter(conceptPair => {
        equalToSentenceWords(conceptPair, sentence, wordId)
      })
    }
  }


  /*==============================
  * Various concept sources
  * ==============================*/
  def termsLookup(input: Input, wordId: Int, onlyPassThrough: Boolean): List[PhraseConceptPair] = {
    val word = input.sentence(wordId)

    def matchesSentenceWords(term: Term): Boolean = {
      val sentenceSlice = input.sentenceLowercased.slice(wordId, wordId + term.words.length).toSeq
      if(term.onlyUpperCase) {
        term.words == sentenceSlice
      } else {
        term.words.map(_.toLowerCase) == sentenceSlice.map(_.toLowerCase)
      }
    }

    val concepts = for (
      term <- termsDict.getOrElse(word, List())
      if term.words.length + wordId <= input.sentence.length
      if matchesSentenceWords(term)
    ) yield {
      PhraseConceptPair(
        words = term.words.toList,
        graphFrag = term.concept,
        features = FeatureVectorBasic(m.Map(
          "TermDict" -> 1.0
        ))
      )
    }

    if (onlyPassThrough) {
      concepts.foreach(_.features.fmap("TermDictOnly") = 1.0)
    }

    concepts
  }


  private def ontoNotesLookup(input: Input, wordId: Int, onlyPassThrough: Boolean): List[PhraseConceptPair] = {
    val stems = Wordnet.getStemms(input.sentence(wordId))

    //TODO: NAUMENKO: nice...so we should only use the first sense? Should do something with this.
    val concepts = stems.filter(ontoNotes.contains).map(stem => {
      PhraseConceptPair(
        words = List(input.sentence(wordId)),
        graphFrag = stem + "-01", // first sense is most common
        features = FeatureVectorBasic(m.Map("OntoNotes" -> 1.0))
      )
    })

    if (onlyPassThrough) {
      concepts.foreach(_.features.fmap("OntoNotesOnly") = 1.0)
    }

    concepts
  }

  private def NEPassThrough(input: Input, wordId: Int, onlyPassThrough: Boolean): List[PhraseConceptPair] = {
    // TODO: improve this to check if the words were observed other places
    var concepts = List[PhraseConceptPair]()

    val possibleNerLengthRange = Range(1, 7)
    for {offset <- possibleNerLengthRange
         if wordId + offset < input.sentence.length
         words = input.sentence.slice(wordId, wordId + offset).toList
         if words.forall(isPossibleNerToken)} {

      val graphFrag = if (optionsStage1Wiki) {
        "(thing :wiki - :name (name " + words.map(x => ":op " + x).mkString(" ") + "))"
      } else {
        "(thing :name (name " + words.map(word => ":op " + word).mkString(" ") + "))"
      }

      concepts = PhraseConceptPair(
        words,
        graphFrag,
        FeatureVectorBasic(m.Map("NEPassThrough" -> 1.0, "NEPassThrough_len" -> offset))
      ) :: concepts
    }

    if (onlyPassThrough) {
      concepts.foreach(x => x.features.fmap("NEPassThroughOnly") = 1.0)
    }

    concepts
  }

  private def isPossibleNerToken(token: String) = {
    token.matches("[A-Za-z0-9.-]*") // TODO: improve this regex
  }
  private def booleanToInt(value: Boolean) = if (value) 1 else 0

  private def passThrough(input: Input, wordId: Int, onlyPassThrough: Boolean): List[PhraseConceptPair] = {
    // TODO: improve this regex
    val word = input.sentence(wordId)
    if (word.matches("[A-Za-z0-9]*")) {
      List(PhraseConceptPair(
        words = List(word),
        graphFrag = word,
        features = FeatureVectorBasic(m.Map(
          "PassThrough" -> 1.0,
          "PassThroughOnly" -> booleanToInt(onlyPassThrough)
        ))
      ))
    } else {
      List()
    }
  }

  private def wordnetPassThrough(input: Input, wordId: Int, onlyPassThrough: Boolean): List[PhraseConceptPair] = {
    val word = input.sentence(wordId)
    val stems = Wordnet.getStemms(word)
    // TODO: add stems from large annotated corpus
    if (stems.nonEmpty) {
      List(PhraseConceptPair(
        List(word),
        stems.minBy(_.length),
        FeatureVectorBasic(m.Map(
          "WordnetPassThrough" -> 1.0,
          "WordnetPassThroughOnly" -> booleanToInt(onlyPassThrough)
        ))
      ))
    } else {
      List()
    }
  }

  private def verbs(input: Input, wordId: Int, onlyPassThrough: Boolean): List[PhraseConceptPair] = {
    var concepts = List[PhraseConceptPair]()
    val posArray = input.pos.slice(wordId, wordId + 1) // NOTE: posArray.slice is defined in Annotation.slice
    val isVerb = posArray.length > 0 && posArray.last.startsWith("V")

    if (isVerb) {
      val word = input.sentence(wordId)
      val stems = Wordnet.getStemms(word)
      val stem = if (stems.nonEmpty) {
        stems.minBy(_.length)
      } else {
        word
      }

      // TODO: check in large corpus
      concepts = List(PhraseConceptPair(
        List(word),
        stem + "-00", // 00 sense for missing predicates
        FeatureVectorBasic(m.Map(
          "AddedVerb" -> 1.0,
          "AddedVerbOnly" -> booleanToInt(onlyPassThrough)
        ))
      ))
    }

    concepts
  }

  private def nominalizations(input: Input, i: Int, onlyPassThrough: Boolean): List[PhraseConceptPair] = {
    // (no change) budget -> budget-01

    // (drop -e in predicate that ends in -ate) proliferate-01 -> proliferation, state-01 -> statement
    // (drop -ify in predicate) intensify-01 -> intensity, ratify-01 -> ratification

    // (drop -ance or -ances) assistance -> assist-01
    // (drop -ment or -ments) development -> develop-02
    // (drop -ing) discriminating -> discriminate-02 (also drop -e)
    // (drop -ion) discrimination -> discriminate-02, discussion -> discuss-01
    // (drop -s) addicts -> addict-01, arrests -> arrest-01
    // (drop -ant or -ants) combatants -> combat-01
    // (drop -ure) seizure -> seize-01, departure -> depart-01, failure -> fail-01 (not always tho: manufacture -> manufacture-01)
    // not as common: (drop -ation) determination -> determine-01 (lots of counter-examples: exaggeration -> exaggerate-01)
    // not common (-ees) employees -> employ-01, attendees -> attend-01

    // -er: parser -> thing :ARG0-of parse-00 (not very common)
    List()
  }

  // verbalization (modern -> modernize (to make modern), etc, not use in AMR modernize -> modernize-01)

  private def namedEntity(input: Input, entity: Entity): PhraseConceptPair = {
    val Input(_, sentence, notTokenized, _, _, ner, _) = input
    val entityType = entity.label match {
      case "PER" => "person" // also president
      case "ORG" => "organization" // also company, government-organization, criminal-organization
      case "LOC" => "country" // also city, world-region, continent, county
      case "MISC" => "thing" // also treaty, publication, newspaper, product, war
    }
    val (start, end) = ner.getSpan((entity.start, entity.end))
    // start and end in ner.snt, which is the tokenized text
    val (notTokStart, notTokEnd) = notTokenized.getSpan((start, end))
    // start and end in notTokenized.snt, which is the original untokenized text

    val wikiOptStr = if (optionsStage1Wiki) ":wiki -" else ""
    val slice = notTokenized.snt.slice(notTokStart, notTokEnd)
    // there should be no " in named entities (TODO: does the AMR style guide say if you can escape them?)
    val nameOps = slice
      .map(_.replaceAllLiterally("\"", ""))
      .map(x => s""":op "$x"""")
      .mkString(" ")
    val graphFrag = s"($entityType$wikiOptStr :name (name $nameOps))"

    logger(0, "NER Entity: " + graphFrag)
    //logger(1, "(start, end) = "+(start,end))
    //logger(1, "ner.snt = "+ner.snt.toList)
    //logger(1, "ner.tok = "+ner.tok.toList)
    //logger(1, "notTokenized.snt = "+notTokenized.snt.toList)
    //logger(1, "notTokenized.tok = "+notTokenized.tok.toList)
    PhraseConceptPair(
      sentence.slice(start, end).toList,
      graphFrag,
      FeatureVectorBasic(m.Map(
        "ner" -> 1.0,
        "ner_len" -> (end - start)
      ))
    )
  }

  private def dateEntities(input: Input, start: Int): List[PhraseConceptPair] = {
    logger(2, "Finding date entities")
    var list: ArrayBuffer[PhraseConceptPair] = ArrayBuffer()
    tokens = input.sentence.drop(start)
    val string = tokens.mkString("\t")
    var monthRegex = "January|February|March|April|May|June|July|August|September|October|November|December|(?:Jan|Feb|Mar|Apr|Jun|Jul|Aug|Sept?|Oct|Nov|Dec)[.]?"
    monthRegex = monthRegex + "|" + monthRegex.toLowerCase

    // 021114 => (date-entity :day 14 :month 11 :year 2002)
    val SixDigitDate =
      """(([0-9][0-9])([0-9][0-9])([0-9][0-9]))(?:\t.*)?""".r // (?: ) non-capturing group
    if (SixDigitDate.pattern.matcher(string).matches) {
      list += {
        var SixDigitDate(matching, year, month, day) = string
        if (year.toInt < 40) {
          year = "20" + year
        } else {
          year = "19" + year
        }
        if (day == "00" && month == "00") {
          mkYear(matching, year) // 170000 => (date-entity :year 2017)
        } else if (day == "00") {
          mkMonthYear(matching, month, year) // 021100 => (date-entity :month 11 :year 2002)
        } else {
          mkDayMonthYear(matching, day, month, year)
        } // 021114 => (date-entity :day 14 :month 11 :year 2002)
      }
    }

    // 17 July 2003 => (date-entity :day 17 :month 7 :year 2003)
    val DayMonthYear = ("""(([0-9]?[0-9])\t(""" + monthRegex +""")\t([0-9][0-9][0-9][0-9]))(?:\t.*)?""").r // (?: ) non-capturing group
    if (DayMonthYear.pattern.matcher(string).matches) {
      list += {
        var DayMonthYear(matching, day, month, year) = string
        mkDayMonthYear(matching, day, month, year)
      }
    }

    // July 2003 => (date-entity :month 7 :year 2003)
    val MonthYear = ("((" + monthRegex +""")\t([0-9][0-9][0-9][0-9]))(?:\t.*)?""").r
    if (MonthYear.pattern.matcher(string).matches) {
      list += {
        var MonthYear(matching, month, year) = string
        mkMonthYear(matching, month, year)
      }
    }

    // July 18 , 2008 => (date-entity :day 18 :month 7 :year 2008)
    val MonthDayYear = ("((" + monthRegex +""")\t?([0-9][0-9]?)\t?,?\t([0-9][0-9][0-9][0-9]))(?:\t.*)?""").r
    if (MonthDayYear.pattern.matcher(string).matches) {
      list += {
        var MonthDayYear(matching, month, day, year) = string
        mkDayMonthYear(matching, day, month, year)
      }
    }

    // 2007-02-27 => (date-entity :day 27 :month 2 :year 2007)
    // 20030106 => (date-entity :day 6 :month 1 :year 2003)
    val EightDigitDate = """(([0-9]?[0-9][0-9]?[0-9])\t?[.-]?\t?([0-9][0-9])\t?[.-]?\t?([0-9][0-9]))(?:\t.*)?""".r // (?: ) non-capturing group
    if (EightDigitDate.pattern.matcher(string).matches) {
      list += {
        var EightDigitDate(matching, year, month, day) = string
        mkDayMonthYear(matching, day, month, year)
      }
    }

    // 1713 => (date-entity :year 1713)
    val Year =
      """(([0-9][0-9][0-9][0-9]))(?:\t.*)?""".r
    if (Year.pattern.matcher(string).matches) {
      list += {
        var Year(matching, year) = string
        mkYear(matching, year)
      }
    }

    // March => (date-entity :month 3)
    val Month = ("((" + monthRegex +"""))(?:\t.*)?""").r
    if (Month.pattern.matcher(string).matches) {
      list += {
        var Month(matching, month) = string
        mkMonth(matching, month)
      }
    }

    list.toList
  }

  private def mkDayMonthYear(matching: String, day: String, month: String, year: String): PhraseConceptPair = {
    //logger(0, "mkDayMonthYear("+matching+","+day+","+month+","+year+")")
    PhraseConceptPair(tokens.take(matching.count(_ == '\t') + 1).toList,
                      "(date-entity :day " + day.toInt.toString + " :month " + monthStr(month) + " :year " + year + ")",
                      FeatureVectorBasic(m.Map("datex1" -> 1.0, "datex_len" -> (matching.count(_ == '\t') + 1))))
  }

  private def mkMonthYear(matching: String, month: String, year: String): PhraseConceptPair = {
    PhraseConceptPair(tokens.take(matching.count(_ == '\t') + 1).toList,
                      "(date-entity :month " + monthStr(month) + " :year " + year + ")",
                      FeatureVectorBasic(m.Map("datex2" -> 1.0, "datex_len" -> (matching.count(_ == '\t') + 1))))
  }

  private def mkMonth(matching: String, month: String): PhraseConceptPair = {
    PhraseConceptPair(tokens.take(matching.count(_ == '\t') + 1).toList,
                      "(date-entity :month " + monthStr(month) + ")",
                      FeatureVectorBasic(m.Map("datex3" -> 1.0, "datex_len" -> (matching.count(_ == '\t') + 1))))
  }

  private def mkYear(matching: String, year: String): PhraseConceptPair = {
    PhraseConceptPair(tokens.take(matching.count(_ == '\t') + 1).toList,
                      "(date-entity :year " + year + ")",
                      FeatureVectorBasic(m.Map("datex4" -> 1.0, "datex_len" -> (matching.count(_ == '\t') + 1))))
  }

  private def monthStr(month: String): String = {
    if (month.matches("[0-9]*")) {
      month.toInt.toString
    } else {
      month.take(3).toLowerCase match {
        case "jan" => "1"
        case "feb" => "2"
        case "mar" => "3"
        case "apr" => "4"
        case "may" => "5"
        case "jun" => "6"
        case "jul" => "7"
        case "aug" => "8"
        case "sep" => "9"
        case "oct" => "10"
        case "nov" => "11"
        case "dec" => "12"
        case _ => month
      }
    }
  }

}

