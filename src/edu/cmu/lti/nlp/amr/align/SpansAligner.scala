package edu.cmu.lti.nlp.amr.align

import edu.cmu.lti.nlp.amr.graph.{Graph, Node}
import edu.cmu.lti.nlp.amr.span.Span
import edu.cmu.lti.nlp.amr.utils.JAMRLogger
import edu.cmu.lti.nlp.amr.{max, min}

import scala.collection.mutable.{ArrayBuffer, Set}

/** **************************** Align Spans *****************************/
case class SpansAligner(logger: JAMRLogger) {
  private val log = logger.log _

  def logUnalignedConcepts(node: Node) {
    if (node.spans.isEmpty) {
      log(1, "WARNING: Unaligned concept " + node.concept)
    }
    for ((_, child) <- node.topologicalOrdering) {
      logUnalignedConcepts(child)
    }
  }

  def alignSpans(sentence: Array[String],
                 /*stemmedSentence: Array[List[String]],*/
                 graph: Graph,
                 wordAlignments: Array[Option[Node]]): Array[Option[Int]] = {
    val spanAlignments = new Array[Option[Int]](sentence.length)
    for (i <- sentence.indices) {
      spanAlignments(i) = None
    }
    createSpans(sentence, /*stemmedSentence,*/ graph.root, wordAlignments, spanAlignments, None, graph.spans)
    log(3, graph.spans.toString)

    spanAlignments
  }

  val specialRelations1 = List(":ARG.*-of")
  val specialRelations2 = List(":unit")
  val specialConcepts = Set(
    "name", "country", "person", "date-entity", "organization", "city", "thing", "company", "monetary-quantity", "continent", "mass-quantity", "religious-group", "political-party", "distance-quantity", "criminal-organization", "research-institute", "date-interval", "temporal-quantity", "world-region", "ethnic-group", "university"
  )
  // "govern-01"

  // Returns the span for 'node'
  //Span(var start: Int, var end: Int, var nodeIds: List[String], var words: String, var amr: Node
  //Node(var id: String, name: Option[String], concept: String, var relations: List[(String, Node)], var topologicalOrdering: List[(String, Node)], var variableRelations: List[(String, Node)], var alignment: Option[Int], var span: Option[Int])
  def createSpans(sentence: Array[String],
                  /*stemmedSentence: Array[List[String]],*/
                  node: Node,
                  wordAlignments: Array[Option[Node]],
                  spanAlignments: Array[Option[Int]],
                  spanIndex: Option[Int],
                  spans: ArrayBuffer[Span]): Option[Span] = {
    val mySpan = Span(sentence.length, 0, List(node.id), "", Node("", node.name, node.concept, List[(String, Node)](), List[(String, Node)](), List[(String, Node)](), None, ArrayBuffer()), coRef = false)

    var valid = false // will update later

    if (specialConcepts.contains(node.concept)) {
      processSpecialConcepts()
    } else {
      processCommonConcepts()
    }

    def processSpecialConcepts() = {
      var mySpanIndex = spanIndex
      if (spanIndex.isEmpty) {
        mySpanIndex = Some(spans.size)
        spans.append(mySpan) // so we can pass a valid spanIndex
      }
      for ((relation, child) <- node.topologicalOrdering) {
        val span = createSpans(sentence, /*stemmedSentence,*/ child, wordAlignments, spanAlignments, mySpanIndex, spans)
        if (span.isDefined) {
          val Some(Span(start, end, nodeIds, _, amr, _)) = span // TODO: is this code right?
          mySpan.start = min(mySpan.start, start)
          mySpan.end = max(mySpan.end, end)
          mySpan.nodeIds = mySpan.nodeIds ::: nodeIds
          mySpan.amrNode.topologicalOrdering = (relation, amr) :: mySpan.amrNode.topologicalOrdering
          mySpan.amrNode.relations = (relation, amr) :: mySpan.amrNode.relations
        }
      }
      mySpan.amrNode.topologicalOrdering = mySpan.amrNode.topologicalOrdering.reverse
      mySpan.amrNode.relations = mySpan.amrNode.relations.reverse
      // TODO: check that the span is valid and update spanAlignments
      valid = true
      for (i <- Range(mySpan.start, mySpan.end)) {
        if (spanAlignments(i).isDefined) {
          if (spanAlignments(i) != mySpanIndex) {
            valid = false // there's a word in the span aligned to a different span, so this is not a valid span
          }
        }
      }
      mySpan.words = sentence.slice(mySpan.start, mySpan.end).mkString(" ")
      if (spanIndex.isEmpty) {
        // we need to save the span
        val Some(index) = mySpanIndex
        spans(index) = mySpan
      }
      if (mySpanIndex.isDefined) {
        // replaces node.spans = mySpanIndex
        val Some(myspanindex) = mySpanIndex
        if (node.spans.isEmpty) {
          node.spans += myspanindex
        } else {
          node.spans(0) = myspanindex
        }
      }
    }

    def processCommonConcepts() = {
      if (node.alignment.isDefined) {
        val Some(alignment) = node.alignment
        mySpan.start = alignment
        mySpan.end = alignment + 1
        mySpan.words = sentence(alignment)
        if (spanIndex.isEmpty) {
          // we need to insert the span ourselves
          spanAlignments(alignment) = Some(spans.size)
          spans.append(mySpan)
        } else {
          spanAlignments(alignment) = spanIndex // index to this span
        }
        if (node.spans.isEmpty) {
          node.spans += spans.size
        } else {
          node.spans(0) = spans.size // TODO: check to see if there are other spans already?
        }
        valid = true
      }
      for ((relation, child) <- node.topologicalOrdering) {
        createSpans(sentence, /*stemmedSentence,*/ child, wordAlignments, spanAlignments, None, spans)
      }
    }

    Some(mySpan).filter(_ => valid)
  }

}

