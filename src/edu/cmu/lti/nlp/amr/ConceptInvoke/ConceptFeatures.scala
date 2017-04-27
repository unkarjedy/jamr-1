package edu.cmu.lti.nlp.amr.ConceptInvoke

import edu.cmu.lti.nlp.amr.BasicFeatureVector._
import edu.cmu.lti.nlp.amr._

import scala.collection.{immutable => i, mutable => m}

/** ************************** Feature Functions *****************************/
// TODO: Would this be faster if the featureFunctionsMap was replaced with boolean variables and lots of if statements?
// TODO: the input to the feature function is just Input and Span.  Change to use Span?

class ConceptFeatures(featureNames: List[String], phraseCounts: i.Map[List[String], Int]) {
  var weights = FeatureVectorBasic()

  // input, phraseConceptPair, start, end
  type FeatureFunction = (Input, PhraseConceptPair, Int, Int) => FeatureVectorBasic

  /** ******************
    * Features to add:
    *   - Fragtype feature (is it an event, named entity, number, string constant, other fragment
    *   - Fragtype and POS tag
    *   - Concept bigrams, and concepts w/o sense tags
    *   - Edge type to named entity
    *   TODO: NAUMENKO
    *   TODO: Concept fragment source
    * ******************/
  private val featureFunctionsMap = m.Map[String, FeatureFunction](
    "bias" -> ffBias,
    "length" -> ffLength,
    "firstMatch" -> ffFirstMatch,
    "numberIndicator" -> ffNumberIndicator,
    "badConcept" -> ffBadConcept,
    "sentenceMatch" -> ffSentenceMatch,
    "andList" -> ffAndList,
    "pos" -> ffPOS,
    "posEvent" -> ffPOSEvent,
    "phrase" -> ffPhrase,
    "phraseConceptPair" -> ffPhraseConceptPair,
    "phraseConceptPairPOS" -> ffPhraseConceptPairPOS,
    "pairWith2WordContext" -> ffPairWith2WordContext
  )

  def ffBias(input: Input, concept: PhraseConceptPair, start: Int, end: Int): FeatureVectorBasic = {
    FeatureVectorBasic(m.Map("bias" -> 1.0))
  }

  def ffLength(input: Input, concept: PhraseConceptPair, start: Int, end: Int): FeatureVectorBasic = {
    FeatureVectorBasic(m.Map("len" -> concept.words.size))
  }

  def ffFirstMatch(input: Input, concept: PhraseConceptPair, start: Int, end: Int): FeatureVectorBasic = {
    if (input.sentence.indexOfSlice(concept.words) == start) {
      FeatureVectorBasic(m.Map("firstMatch" -> 1.0))
    } else {
      FeatureVectorBasic()
    }
  }

  def ffNumberIndicator(input: Input, concept: PhraseConceptPair, start: Int, end: Int): FeatureVectorBasic = {
    if (concept.words.size == 1 && concept.words.head.matches("[0-9]*") && concept.words.head == concept.graphFrag) {
      FeatureVectorBasic(m.Map("numIndicator" -> 1.0))
    } else {
      FeatureVectorBasic()
    }
  }

  def ffBadConcept(input: Input, concept: PhraseConceptPair, start: Int, end: Int): FeatureVectorBasic = {
    if (concept.graphFrag.matches("[A-Za-z]*") && concept.graphFrag.length <= 2) {
      FeatureVectorBasic(m.Map("badConcept" -> 1.0))
    } else {
      FeatureVectorBasic()
    }
  }

  def ffSentenceMatch(input: Input, concept: PhraseConceptPair, start: Int, end: Int): FeatureVectorBasic = {
    if (input.sentence.length == concept.words.size) {
      FeatureVectorBasic(m.Map("sentenceMatch" -> 1.0))
    } else {
      FeatureVectorBasic()
    }
  }

  def ffAndList(input: Input, concept: PhraseConceptPair, start: Int, end: Int): FeatureVectorBasic = {
    // ; separated list
    if (input.sentence(start) == ";" && input.sentence.mkString(" ").matches("[^;]+(?: ; .*)+[^.!?]$")) {
      // TODO: Naumenko: why start + 1?? what about longer lists?
      if (start == input.sentence.indexOf(";") && end == start + 1) {
        FeatureVectorBasic(m.Map("andList" -> 1.0))
      } else {
        FeatureVectorBasic(m.Map("andListNot" -> 1.0))
      }
    } else {
      FeatureVectorBasic()
    }
  }

  def ffPOS(input: Input, concept: PhraseConceptPair, start: Int, end: Int): FeatureVectorBasic = {
    val partOfSpeech = "POS=" + input.pos.slice(start, end).mkString("_")
    FeatureVectorBasic(m.Map(partOfSpeech -> 1.0))
  }

  def ffPOSEvent(input: Input, concept: PhraseConceptPair, start: Int, end: Int): FeatureVectorBasic = {
    val partOfSpeech = "POS=" + input.pos.slice(start, end).mkString("_")
    val event = if (concept.graphFrag.matches(".*-[0-9][0-9]")) {
      "T"
    } else {
      "F"
    }
    FeatureVectorBasic(m.Map(partOfSpeech + "+EVENT=" + event -> 1.0))
  }

  def ffPhrase(input: Input, concept: PhraseConceptPair, start: Int, end: Int): FeatureVectorBasic = {
    if (phraseCounts.getOrElse(concept.words, 0) > 10) {
      FeatureVectorBasic(m.Map("phrase=" + concept.words.mkString("_") -> 1.0))
    } else {
      FeatureVectorBasic()
    }
  }

  def ffPhraseConceptPair(input: Input, concept: PhraseConceptPair, start: Int, end: Int): FeatureVectorBasic = {
    if (concept.trainingIndices.size > 10) {
      FeatureVectorBasic(m.Map("CP=" + concept.words.mkString("_") + "=>" + concept.graphFrag.replaceAllLiterally(" ", "_") -> 1.0))
    } else {
      FeatureVectorBasic()
    }
  }

  def ffPhraseConceptPairPOS(input: Input, concept: PhraseConceptPair, start: Int, end: Int): FeatureVectorBasic = {
    if (concept.trainingIndices.size > 3) {
      FeatureVectorBasic(m.Map("CP=" + concept.words.mkString("_") + "+POS=" + input.pos.slice(start, end).mkString("_") + "=>" + concept.graphFrag.replaceAllLiterally(" ", "_") -> 1.0))
    } else {
      FeatureVectorBasic()
    }
  }

  def ffPairWith2WordContext(input: Input, concept: PhraseConceptPair, start: Int, end: Int): FeatureVectorBasic = {
    val cp = "CP=" + concept.words.mkString("_") + "=>" + concept.graphFrag.replaceAllLiterally(" ", "_")
    val feats = FeatureVectorBasic()
    if (start > 0) {
      feats.fmap(cp + "+" + "W-1=" + input.sentence(start - 1)) = 1.0
    }
    if (end < input.sentence.length) {
      feats.fmap(cp + "+" + "W+1=" + input.sentence(end)) = 1.0
    }
    feats
  }

  private val featureFunctions: List[FeatureFunction] = {
    featureNames.filter(featureFunctionsMap.contains).map(featureFunctionsMap)
  }
  // TODO: error checking on lookup
  private val unknownFeatures = {
    featureNames.filterNot(isKnownFeature)
  }

  assert(unknownFeatures.isEmpty, "Unknown stage1 features: " + unknownFeatures.mkString(","))

  private def isKnownFeature(featureName: String): Boolean = {
    featureFunctionsMap.contains(featureName) || ExtractConceptTable.implementedFeatures.contains(featureName) || Concepts.implementedFeatures.contains(featureName)
  }

  // Calculate the local features
  def localFeatures(input: Input, concept: PhraseConceptPair, start: Int, end: Int): FeatureVectorBasic = {
    val feats = FeatureVectorBasic()
    for (ff <- featureFunctions) {
      feats += ff(input, concept, start, end)
    }
    feats += concept.features // add the features in the rule
    feats
  }

  def localScore(input: Input, concept: PhraseConceptPair, start: Int, end: Int): Double = {
    var score = 0.0
    for (ff <- featureFunctions) {
      score += weights.dot(ff(input, concept, start, end))
    }
    score += weights.dot(concept.features) // add the features in the rule
    score
  }

}