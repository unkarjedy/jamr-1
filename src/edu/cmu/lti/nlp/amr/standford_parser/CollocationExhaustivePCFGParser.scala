package edu.cmu.lti.nlp.amr.standford_parser

import java.util.{List => JList}

import edu.stanford.nlp.ling.HasWord
import edu.stanford.nlp.parser.lexparser._
import edu.stanford.nlp.util.Index

class CollocationExhaustivePCFGParser private(myGrammar: MyGrammar,
                                              collocationDictionary: CollocationDictionary,
                                              ////////////////
                                              bg: BinaryGrammar,
                                              ug: UnaryGrammar,
                                              lex: Lexicon,
                                              op: Options,
                                              stateIndex: Index[String],
                                              wordIndex: Index[String],
                                              tagIndex: Index[String])
  extends ExhaustivePCFGParser(bg, ug, lex, op, stateIndex, wordIndex, tagIndex) {

  protected val maxSpanForTags: Int = 5 // needts to be determined from collocation dictionary
  protected val baseLexicon: BaseLexicon = myGrammar.lex


//  /*override */
//  protected def initializeChart2(sentence: java.util.List[_ <: HasWord]): Unit = {
//    val boundary = wordIndex.indexOf(Lexicon.BOUNDARY)
//    var start = 0
//    while (start + 1 <= length) {
//      if (maxSpanForTags > 1) { // only relevant for parsing single words as multiple input tokens.
//        // EGOR_INFO: Code has some differences from Stanford code, which are made because of
//        // EGOR_INFO: our knowledge about collocations, so if we met a word, which can be considered
//        // EGOR_INFO: as first word in collocations we try to capture that collocation in code below.
//        // EGOR_INFO: We append sentence words only in case if all of them could be found in collocations
//        // EGOR_INFO: words dictionary...
//        // note we don't look for "words" including the end symbol!
//        // TODO NAUMENKO NOTE: why not considering ParserConstraint.java that seams to allow us to detect collocations before we start parsing the sentence?
//        var end = start + 1
//        while ((end < length && end - start <= maxSpanForTags) || (start + 1 == end)) {
//          val collocationStr = new StringBuilder
//          //wsg: Feb 2010 - Appears to support character-level parsing
//
//          val collocationStrNormalized = (start until end).map(sentence.get)
//            .map {
//              case cl: HasWord => cl.asInstanceOf[HasWord].word
//              case word => word.toString
//            }
//            .mkString("")
//            .trim.toLowerCase.replaceAll("\\s+", " ")
//
//
//          val collocation = Collocation(collocationStrNormalized)
//          var tagId = 0
//          while (tagId < tagIndex.size) {
//            val tag = tagIndex.get(tagId)
//            val state = stateIndex.indexOf(tag)
//            val iS = iScore(start)(end)(state)
//            if (iS == Float.NegativeInfinity) {
//              // scoring
//              // TODO
//              //              iScore(start)(end)(state) = baseLexicon.score(
//              //                new IntTaggedWord(collocation.str, collocation.POS, wordIndex, tagIndex),
//              //                collocation.strGlued)
//              if (iScore(start)(end)(state) > Float.NegativeInfinity) {
//                narrowRExtent(start)(state) = start + 1
//                narrowLExtent(end)(state) = end - 1
//                wideRExtent(start)(state) = start + 1
//                wideLExtent(end)(state) = end - 1
//              }
//            }
//
//            tagId += 1
//          }
//          // EGOR_INFO: I've added unaries rules adding, because there was no way
//          // EGOR_INFO: to construct, for example, NP over NN (i.e. apply rule NP --> NN)
//          var state = 0
//          while (state < numStates) {
//            val iS = iScore(start)(end)(state) // EGOR: here we also may get Parent nodes, not only tags...
//            if (iS != Float.NegativeInfinity) {
//              val unaries = ug.closedRulesByChild(state)
//              for (ur <- unaries) {
//                val parentState = ur.parent
//                val pS = ur.score
//                val tot = iS + pS
//                if (tot > iScore(start)(end)(parentState)) {
//                  iScore(start)(end)(parentState) = tot
//                  narrowRExtent(start)(parentState) = start + 1
//                  narrowLExtent(end)(parentState) = end - 1
//                  wideRExtent(start)(parentState) = start + 1
//                  wideLExtent(end)(parentState) = end - 1
//                }
//              }
//            }
//            state += 1
//          }
//
//          end += 1
//        }
//      } else { // "normal" chart initialization of the [start,start+1] cell
//        throw new RuntimeException("here should be: `normalTaggingInit(sentence, boundary, start)`")
//      }
//
//      start += 1
//    }
//  }

}

object CollocationExhaustivePCFGParser {
  def apply(grammar: MyGrammar, collocationDictionary: CollocationDictionary): CollocationExhaustivePCFGParser = {
    new CollocationExhaustivePCFGParser(
      grammar, collocationDictionary,
      grammar.bg,
      grammar.ug,
      grammar.lex,
      grammar.op,
      grammar.stateIndex,
      grammar.wordIndex,
      grammar.tagIndex
    )
  }
}



