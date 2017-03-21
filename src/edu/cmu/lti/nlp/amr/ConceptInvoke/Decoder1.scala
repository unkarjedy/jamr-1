package edu.cmu.lti.nlp.amr.ConceptInvoke

import edu.cmu.lti.nlp.amr.BasicFeatureVector._
import edu.cmu.lti.nlp.amr._
import edu.cmu.lti.nlp.amr.graph.Graph

import scala.collection.{immutable => i, mutable => m}

class Decoder1(options: m.Map[Symbol, String],
               featureNames: List[String],
               phraseConceptPairs: Array[PhraseConceptPair],
               phraseCounts: i.Map[List[String], Int])
  extends Decoder(featureNames, phraseCounts) {

  private val conceptInvoker = new Concepts(options, phraseConceptPairs)

  def decode(input: Input,
             trainingIndex: Option[Int], // if we are training, index into the training data so we can do leave-one-out decoding
             cost: (Input, PhraseConceptPair, Int, Int, List[PhraseConceptPair]) => Double): DecoderResult = {

    logger(1, "\n--- Decoder1 ---\n")
    logger(1, "Sentence: " + input.sentence.mkString(" "))
    //logger(1, "Weights:\n"+features.weights.toString)

    case class State(score: Double, concept: PhraseConceptPair, backPointer: Int)

    val sentence = input.sentence
    val bestState: Array[Option[State]] = sentence.map(x => None)

    for (wordIdx <- sentence.indices) {
      logger(2, "word = " + sentence(wordIdx))
      var conceptList = conceptInvoker.invoke(input, wordIdx, trainingIndex)
      //logger(1, "Possible invoked concepts: "+conceptList.map(x => x.toString).mkString("\n"))

      // WARNING: the code below assumes that anything in the conceptList will not extend beyond the end of the sentence (and it shouldn't based on the code in Concepts)
      for (concept <- conceptList) {
        if (concept.words.size + wordIdx > sentence.length) {
          logger(0, "WARNING: concept fragment " + concept.graphFrag + " extends beyond the end of the sentence - I will ignore it.")
        } else {
          val score = (features.localScore(input, concept, wordIdx, wordIdx + concept.words.size)
            + cost(input, concept, wordIdx, wordIdx + concept.words.size, conceptList))
          //logger(1, "concept = "+concept.graphFrag)
          val endpoint = wordIdx + concept.words.size - 1
          //logger(2, "score = "+score.toInt)
          if ((bestState(endpoint).isEmpty && score >= 0) || (bestState(endpoint).isDefined && bestState(endpoint).get.score <= score)) {
            // we use <= so that earlier concepts (wordIdx.e. ones our conceptTable) have higher priority
            bestState(endpoint) = Some(State(score, concept, wordIdx))
          }
        }
      }
    }

    logger(2, "Chart = " + bestState.toList)

    // Follow backpointers
    var graph = Graph.Null()
    var score = 0.0
    val feats = FeatureVector()
    var wordIdx = bestState.length - 1
    graph.getNodeById.clear
    graph.getNodeByName.clear
    while (wordIdx >= 0) {
      if (bestState(wordIdx).isDefined) {
        val State(localScore, concept, backpointer) = bestState(wordIdx).get
        //logger(1, "Adding concept: "+concept.graphFrag)
        graph.addSpan(sentence, start = backpointer, end = wordIdx + 1, amrStr = concept.graphFrag)
        //logger(1, "words = "+concept.words.mkString(" "))
        val conceptList = conceptInvoker.invoke(input, backpointer, trainingIndex)
        for (c <- conceptList.filter(x => x.words == concept.words && x.graphFrag == concept.graphFrag)) {
          // add features for all matching phraseConceptPairs (this is what the Oracle decoder does, so we do the same here)
          val f = features.localFeatures(input, c, backpointer, backpointer + concept.words.size)
          feats += f
          score += features.weights.dot(f) + cost(input, c, backpointer, backpointer + concept.words.size, conceptList)
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

