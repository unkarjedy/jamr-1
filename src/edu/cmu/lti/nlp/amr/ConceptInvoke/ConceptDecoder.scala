package edu.cmu.lti.nlp.amr.ConceptInvoke

import edu.cmu.lti.nlp.amr.BasicFeatureVector._
import edu.cmu.lti.nlp.amr._
import edu.cmu.lti.nlp.amr.graph.Graph
import edu.cmu.lti.nlp.amr.term.TermsDict

import scala.collection.{immutable => i, mutable => m}

class ConceptDecoder(options: m.Map[Symbol, String],
                     featureNames: List[String],
                     phraseConceptPairs: Array[PhraseConceptPair],
                     phraseCounts: i.Map[List[String], Int])
  extends ConceptDecoderAbstract(featureNames, phraseCounts) {

  private val conceptInvoker = new ConceptInvoker(options, phraseConceptPairs)

  private val usingTermsDict = options.contains('termsDict)
  private def calcExtraTermCost(concept: PhraseConceptPair): Float = {
    if(usingTermsDict && concept.graphFrag.endsWith(TermsDict.TERM_CONCEPT_SUFFIX)) {
      1000000f // dominate terms concepts
    } else {
      0.0f
    }
  }

  def decode(input: Input, trainingIndex: Option[Int], calcExtraCost: ExtraCostFunc): DecoderResult = {
    logger(1, "\n--- Decoder1 ---\n")
    logger(1, "Sentence: " + input.sentence.mkString(" "))

    case class State(score: Double, concept: PhraseConceptPair, backPointer: Int)

    val sentenceWords = input.sentence
    val bestState: Array[Option[State]] = sentenceWords.map(_ => None)

    for (wordId <- sentenceWords.indices) {
      val possibleConcepts = conceptInvoker.invoke(input, wordId, trainingIndex)

      logger(2, "word = " + sentenceWords(wordId))
      logger(1, "Possible invoked concepts: " + possibleConcepts.map(x => x.toString).mkString("\n"))

      // WARNING: the code below assumes that anything in the possibleConcepts
      // will not extend beyond the end of the sentence (and it shouldn't based on the code in ConceptInvoker)
      for (concept <- possibleConcepts) {
        val exeedsSentenceBorders = concept.words.size + wordId > sentenceWords.length
        if (exeedsSentenceBorders) {
          logger(0, "WARNING: concept fragment " + concept.graphFrag + " extends beyond the end of the sentence - I will ignore it.")
        } else {
          val localScore = features.localScore(input, concept, wordId, wordId + concept.words.size)
          val extraCost = calcExtraCost(input, concept, wordId, wordId + concept.words.size, possibleConcepts)
          val extraTermCost = calcExtraTermCost(concept)
          val score = localScore + extraCost + extraTermCost
          val endpoint = wordId + concept.words.size - 1

          // we use <= so that earlier concepts (wordId.e. ones our conceptTable) have higher priority
          // TODO: NAUMENKO: upper comment is weird, you should use < here if you want earlier concepts to have higher priority
          if ((bestState(endpoint).isEmpty && score >= 0) || bestState(endpoint).exists(_.score <= score)) {
            bestState(endpoint) = Some(State(score, concept, wordId))
          }

          //logger(1, "concept = "+concept.graphFrag)
          //logger(2, "score = "+score.toInt)
        }
      }

      logger(1, "")
    }

    logger(2, "Chart = " + bestState.toList)

    // Follow backpointers
    var graph = Graph.Null()
    var score = 0.0
    val feats = FeatureVectorBasic()
    var wordIdx = bestState.length - 1
    graph.getNodeById.clear
    graph.getNodeByName.clear
    while (wordIdx >= 0) {
      if (bestState(wordIdx).isDefined) {
        val State(localScore, concept, backpointer) = bestState(wordIdx).get
        graph.addSpan(sentenceWords, start = backpointer, end = wordIdx + 1, amrStr = concept.graphFrag)
        //logger(1, "Adding concept: "+concept.graphFrag)
        //logger(1, "words = "+concept.words.mkString(" "))
        val conceptList = conceptInvoker.invoke(input, backpointer, trainingIndex)

        // add features for all matching phraseConceptPairs (this is what the Oracle decoder does, so we do the same here)
        for (concept <- conceptList.filter(c => c.words == concept.words && c.graphFrag == concept.graphFrag)) {
          val f = features.localFeatures(input, concept, backpointer, backpointer + concept.words.size)
          feats += f
          score += features.weights.dot(f) + calcExtraCost(input, concept, backpointer, backpointer + concept.words.size, conceptList)
          //logger(2, "\nphraseConceptPair: "+concept.toString)
          //logger(1, "feats:\n"+f.toString)
          //logger(1, "score:\n"+score.toString+"\n")
        }
        //feats += features.localFeatures(input, concept)
        //score += localScore
        wordIdx = backpointer
      }
      wordIdx -= 1
    }

    // if no invoked concepts
    if (graph.getNodeById.isEmpty) {
      graph = Graph.AMREmpty()
    }

    logger(1, "Decoder1 Spans:")
    for ((span, i) <- graph.spans.sortBy(x => x.words.toLowerCase).zipWithIndex) {
      logger(1, "Span " + (i + 1).toString + ":  " + span.words + " => " + span.amrNode)
    }
    logger(1, "Decoder1 feats:\n" + feats.toString)
    DecoderResult(graph, feats, score)
  }

}

