package edu.cmu.lti.nlp.amr.align

import java.util.regex.Pattern

import edu.cmu.lti.nlp.amr.graph.{Graph, Node}
import edu.cmu.lti.nlp.amr.span.{Span, SpanLoader}
import edu.cmu.lti.nlp.amr.utils.JAMRLogger
import edu.cmu.lti.nlp.amr.{Wordnet, max, min}

import scala.collection.mutable.Map
import scala.util.matching.Regex

/** **************************** Align Words *****************************/
case class SpansAlignerWrapper3(logger: JAMRLogger) {
  private val log = logger.log _

  def align(sentence: Array[String], graph: Graph) {
    val stemmedSentence = sentence.map(stemmer)
    val wordToSpan: Array[Option[Int]] = sentence.map(x => None)
    log(3, "Stemmed sentence " + stemmedSentence.toList.toString)

    val namedEntity = new SpanAligner3(sentence, graph) {
      concept = _.matches("name")
      tabSentence = "\t" + sentence.mkString("\t").toLowerCase + "\t" /*.replaceAll("[^a-zA-Z0-9\t]","")*/
      nodes = node => {
        if (node.children.exists(_._1.matches(":op.*"))) {
          ("", node) :: node.children.filter(_._1.matches(":op.*"))
        } else {
          List()
        }
      }

      words = nodes => {
        ("\t" + nodes.tail.map(x => getConcept(x._2.concept).toLowerCase /*.replaceAll("[^a-zA-Z0-9\t]","")*/ .map(x => Pattern.quote(x.toString)).mkString("\t?")).mkString("[^a-zA-Z]*") + "\t").r
      }
    }

    val namedEntityAcronym = new SpanAligner3(sentence, graph) {     // Matches an acronym that is the first letter of every :op child
      concept = _.matches("name")
      tabSentence = "\t"+sentence.mkString("\t").toLowerCase+"\t"/*.replaceAll("[^a-zA-Z0-9\t]","")*/
      nodes = node => {
        if (node.children.exists(_._1.matches(":op.*"))) {
          ("", node) :: node.children.filter(_._1.matches(":op.*"))
        } else {
          List()
        }
      }
      //words = nodes => { nodes.tail.map(x => Pattern.quote(getConcept(x._2.concept).toLowerCase.replaceAll("[^a-zA-Z0-9\t]",""))).mkString("[^a-zA-Z]*").r }
      words = nodes => { ("\t"+nodes.tail.map(x => getConcept(x._2.concept).head.toString.toLowerCase/*.replaceAll("[^a-zA-Z0-9\t]","")*/.map(x => Pattern.quote(x.toString)).mkString("\t?")).mkString("[^a-zA-Z]*")+"\t").r }
    }

    val fuzzyNamedEntity = new SpanAligner3(sentence, graph) {
      concept = _.matches("name")
      tabSentence = "\t" + sentence.mkString("\t").toLowerCase + "\t" /*.replaceAll("[^a-zA-Z0-9\t]","")*/
      nodes = node => {
        if (node.children.exists(_._1.matches(":op.*"))) {
          ("", node) :: node.children.filter(_._1.matches(":op.*"))
        } else {
          List()
        }
      }
      words = nodes => {
        ("\t" + (for ((_, node) <- nodes.tail) yield {
          var conceptStr = getConcept(node.concept).toLowerCase
          conceptStr = conceptStr.slice(0, max(fuzzyMatchLength(stemmedSentence, node), 4))
          conceptStr.map(x => Pattern.quote(x.toString)).mkString("\t?")
        }).mkString("[^\t]*[^a-zA-Z]*") + "[^\t]*\t").r
      }
      //map(x => getConcept(x._2.concept).toLowerCase/*.replaceAll("[^a-zA-Z0-9\t]","")*/.map(x => Pattern.quote(x.toString)).mkString("\t?")).mkString("[^\t]*[^a-zA-Z]*")+"[^\t]*\t").r }
    }

    // aligns a NE if all the children have been aligned
    val namedEntityCollect = new SpanUpdater3(sentence, graph, wordToSpan) {
      concept = "name"
      nodes = node => {
        if (node.children.count(x => x._1.matches(":op.*") && isAligned(x._2, graph)) == node.children.size && node.children.size > 0) {
          (("", node) :: node.children) :: node.children.tail.map(x => List())
        } else {
          List()
        }
      }

      // we are always given a list of size > 0
      spanIndex = list => {
        list.head.head._2.children.map(x => getAlignment(x._2, graph))
      }
      spans = (node, spanIndices) => {
        val start = node.children.map(x => graph.spans(getAlignment(x._2, graph)).start).min
        val end = node.children.map(x => graph.spans(getAlignment(x._2, graph)).end).max
        (start, end) :: spanIndices.tail.map(x => (0, 0)) // we are always given a list of size > 0
      }
    }

    val namedEntityCollectSingle = new SpanUpdater3(sentence, graph, wordToSpan) { // aligns a NE if one of the childen has been aligned
      concept = "name"
      nodes = node => {
        if (node.children.count(x => x._1.matches(":op.*")) == node.children.size && node.children.count(x => isAligned(x._2, graph)) == 1 && node.children.size > 0) {
          List(("", node) :: node.children)
        } else {
          List()
        }
      }
      spanIndex = list => { List(getAlignment(list(0)(0)._2.children.find(x => isAligned(x._2, graph)).get._2, graph)) }  // we are always given a list of size > 0 (list(0)(0) is the first node in the nodes list, which is the root node)
      spans = (node, spanIndices) => {
        val span = graph.spans(getAlignment(node.children.find(x => isAligned(x._2, graph)).get._2, graph))
        (span.start, span.end) :: spanIndices.tail.map(x => (0,0))                                // we are always given a list of size > 0
      }
    }

    val dateEntity = new SpanAligner3(sentence, graph) {
      concept = _.matches("date-entity")
      tabSentence = "\t" + replaceAll(sentence.mkString("\t").toLowerCase + "\t",
                                      Map("january" -> "1",
                                          "february" -> "2",
                                          "march" -> "3",
                                          "april" -> "4",
                                          "may" -> "5",
                                          "june" -> "6",
                                          "july" -> "7",
                                          "august" -> "8",
                                          "september" -> "9",
                                          "october" -> "10",
                                          "november" -> "11",
                                          "december" -> "12",
                                          "[^0-9a-zA-Z\t]" -> ""))
      nodes = node => {
        if (node.children.exists(_._1.matches(":year|:month|:day"))) {
          ("", node) :: node.children.filter(_._1.matches(":year|:month|:day"))
        } else {
          List()
        }
      }
      words = nodes => {
        val concepts = nodes.map(x => (x._1, List(x._2.concept)))
        val year = concepts.find(_._1 == ":year").getOrElse(("", List()))._2
        val month = concepts.find(_._1 == ":month").getOrElse(("", List()))._2
        val day = concepts.find(_._1 == ":day").getOrElse(("", List()))._2
        def to2Digit(str: String): String = {
          // Converts to 2 digit format, i.e. "2012" to "12", "7" to "07", "12" to "12", "" to "00"
          (str.reverse.take(2) + "00").slice(0, 2).reverse
        }
        val yearStr = year.lift(0).getOrElse("")
        val monthStr = month.lift(0).getOrElse("")
        val dayStr = day.lift(0).getOrElse("")
        ((year ::: month ::: day).permutations.map("\t0*" + _.mkString("\t*0*") + "\t").mkString("|")
          + "|\t" + to2Digit(yearStr) + to2Digit(monthStr) + to2Digit(dayStr) + "\t").r
      }
    }

    val minusPolarity = new SpanAligner3(sentence, graph) {
      concept = _.matches("-")
      tabSentence = "\t"+sentence.mkString("\t").toLowerCase+"\t"
      var word = List("no", "not", "non", "nt", "n't")     // TODO: Add anti, anti- Anti
      nodes = node => { if (sentence.exists(x=> word.contains(x.toLowerCase))) {
        List(("", node))
      } else {
        List()
      }
      }
      words = nodes => { ("\t" + word.mkString("\t|\t") + "\t").r }
    }

    val singleConcept = new SpanAligner3(sentence, graph) {
      val conceptRegex = "[^-].*".r  // so :polarity - doesn't get matched (:op1 "-" still does thought
      concept = c => conceptRegex.unapplySeq(getConcept(c)).isDefined
      nodes = node => {
        if (alignWord(stemmedSentence, node, wordToSpan) != None) {
          List(("", node))
        } else {
          List()
        }
      }
      spans = nodes => {
        val Some(index) = alignWord(stemmedSentence, nodes(0)._2, wordToSpan)
        List((index, index+1))
      }
    }

    val fuzzyConcept = new SpanAligner3(sentence, graph) {
      val conceptRegex = "[^-].*".r  // so :polarity - doesn't get matched (:op1 "-" still does thought
      val notMatch = "have-org-role-91|have-rel-role-91"
      concept = c => ((conceptRegex.unapplySeq(getConcept(c)) != None) && (notMatch.r.unapplySeq(c) == None))
      nodes = node => {
        if (fuzzyAlign(stemmedSentence, node, wordToSpan).isDefined) {
          List(("", node))
        } else {
          List()
        }
      }
      spans = nodes => {
        val Some(index) = fuzzyAlign(stemmedSentence, nodes.head._2, wordToSpan)
        List((index, index + 1))
      }
    }

    val unalignedEntity = new UnalignedConcept(sentence, graph, wordToSpan) {
      concept = ".*"
      label = ":name"
    }
    val quantity = new UnalignedConcept(sentence, graph, wordToSpan) {
      concept = ".*-quantity"
      label = ":unit"
    }
    val argOf = new UnalignedConcept(sentence, graph, wordToSpan) {
      concept = "person|thing"
      label = ":.*-of"
    }
    // ARG?-of, instrument-of
    val personOf = new UnalignedConcept(sentence, graph, wordToSpan) {
      concept = "person"
      label = ":.*"
      num = 1
    }
    // Koreans = (person :poss ..)
    val governmentOrg = new UnalignedChild(sentence, graph, wordToSpan) {
      concept = "government-organization"
      label = ":ARG.*-of"
    }
    val polarityChild = new UnalignedChild(sentence, graph, wordToSpan) {
      concept = ".*"
      label = ":polarity"
      words = "un.*|in.*|il.*"
    }
    // il.* for illegal // TODO:add anti(-)
    val est = new UnalignedChild(sentence, graph, wordToSpan) {
      concept = ".*"
      label = ":degree"
      words = ".*est"
    }
    val er = new UnalignedChild(sentence, graph, wordToSpan) {
      concept = ".*"
      label = ":degree"
      words = ".*er"
    }
    val US = new ConceptAndChildren(sentence, graph) {
      concept = _.matches("name");
      children = Map(":op1" -> "United",
                     ":op2" -> "States")
      words = x => { "\tus\t|\tu[.]\t?s[.]\t".r }
    }

    // have-org-role-91 or have-rel-role-91 aligned with to ARG2 child
    val haveRoleArg2 = new UnalignedConcept(sentence, graph, wordToSpan) {
      concept = "have-org-role-91|have-rel-role-91"
      label = ":ARG2"
    }
    // have-org-role-91 aligned with to ARG1 child
    val haveOrgRoleArg1 = new UnalignedConcept(sentence, graph, wordToSpan) {
      concept = "have-org-role-91"
      label = ":ARG1"
    }
    val wiki = new UnalignedChild(sentence, graph, wordToSpan) {
      concept = ".*"; label = ":wiki"
    }

    addAllSpans(namedEntity, graph, wordToSpan, addCoRefs = false)
    addAllSpans(fuzzyNamedEntity, graph, wordToSpan, addCoRefs = false)
    //        namedEntity.coRef = true
    //        addAllSpans(namedEntity, graph, wordToSpan, addCoRefs=true)
    addAllSpans(dateEntity, graph, wordToSpan, addCoRefs = false)
    //        dateEntity.coRef = true
    //        addAllSpans(dateEntity, graph, wordToSpan, addCoRefs=true)
    addAllSpans(minusPolarity, graph, wordToSpan, addCoRefs = false)
    addAllSpans(singleConcept, graph, wordToSpan, addCoRefs = false)
    addAllSpans(fuzzyConcept, graph, wordToSpan, addCoRefs = false)
    addAllSpans(US, graph, wordToSpan, addCoRefs = false)
    addAllSpans(namedEntityAcronym, graph, wordToSpan, addCoRefs = false)

    updateSpansSafe(namedEntityCollect, graph)
    updateSpansSafe(namedEntityCollectSingle, graph)
    updateSpansSafe(unalignedEntity, graph)
    updateSpansSafe(haveRoleArg2, graph)
    updateSpansSafe(haveOrgRoleArg1, graph)
    updateSpansSafe(quantity, graph)
    updateSpansSafe(argOf, graph)
    updateSpansSafe(personOf, graph)
    updateSpansSafe(governmentOrg, graph)

    addAllSpans(minusPolarity, graph, wordToSpan, addCoRefs = false)

    updateSpansSafe(polarityChild, graph)
    updateSpansSafe(est, graph)
    updateSpansSafe(haveRoleArg2, graph)
    updateSpansSafe(haveOrgRoleArg1, graph)
    updateSpansSafe(wiki, graph)


    //try { updateSpans(er, graph) } catch { case e : Throwable => Unit }
    //dateEntities(sentence, graph)
    //namedEntities(sentence, graph)
    //specialConcepts(sentence, graph) // un, in, etc
    //singleConcepts(sentence, graph)
    //learnedConcepts(sentence, graph)

    //    spanEntity("""(date-entity)""".r, (node, children) => node :: children.map(_.2), (node, children) => children.map(x => Pattern.quote(getConcept(x._2.concept).toLowerCase)).mkString("[^a-zA-Z]*"))

    // spanEntity(conceptRegex, func_to_produce_nodes, func_to_match_a_span_of_words)

    // newSpan(conceptRegex, func_to_produce_nodes, func_to_match_a_span_of_words, coRefs = true)
    // updateSpan(conceptRegex, pointer_to_span, func_to_produce_nodes, func_to_match_a_span_of_words)

    // newSpan(lcSentence, "name", rep("id.*".r),

  }

  private def addAllSpans(f: SpanAligner3, graph: Graph, wordToSpan: Array[Option[Int]], addCoRefs: Boolean) {
    def add(node: Node) {
      var added = false
      for (span <- f.getSpans(node)) {
        if (!overlap(span, graph, wordToSpan) && (!added || addCoRefs) || (span.coRef && addCoRefs)) {
          added = true
          graph.addSpan(span)
          for (i <- Range(span.start, span.end)) {
            wordToSpan(i) = Some(graph.spans.size - 1)
          }
        }
      }
    }
    graph.doRecursive(add)
  }

  private def updateSpansSafe(f: SpanUpdater3, graph: Graph): Unit = {
    try {
      updateSpans(f, graph)
    } catch {
      case e: Throwable => Unit
    }
  }

  private def updateSpans(f: SpanUpdater3, graph: Graph) {
    graph.doRecursive(f.update)
  }

  class SpanAligner3(val sentence: Array[String],
                     val graph: Graph) {
    var tabSentence: String = ""
    var concept: (String => Boolean) = x => false
    // function that returns the nodes
    var nodes: Node => List[(String, Node)] = x => List()
    // function that returns the words
    var words: (List[(String, Node)]) => Regex = x => "".r
    var coRef = false

    var spans: (List[(String, Node)]) => List[(Int, Int)] = allNodes => {
      val regex = words(allNodes)
      val matchList = regex.findAllMatchIn(tabSentence).toList
      log(2, s"regex: $regex")
      log(2, s"matchList: $matchList")
      matchList.map(x => matchToIndices(x, tabSentence))
    }

    def getSpans(node: Node): List[Span] = {
      log(2, s"Processing node: ${node.concept}")
      node match {
        case Node(_,_,c,_,_,_,_,_) if concept(c) && !node.isAligned(graph) => {
          log(2, s"Matched concept regex: $concept")
          log(2, s"tabSentence: $tabSentence")
          val allNodes = nodes(node)
          log(2, s"allNodes: ${allNodes.map(x => (x._1, getConcept(x._2.concept))).toString}")
          if (allNodes.nonEmpty) {
            val indices = spans(allNodes) // the default implementation of spans calls words
            indices.zipWithIndex.map(x => toSpan(x._1, allNodes.map(_._2.id), sentence, graph, coRef))
          } else {
            List()
          }
        }
        case _ => List()
      }
    }
  }

  class SpanUpdater3(val sentence: Array[String],
                     val graph: Graph,
                     val wordToSpan: Array[Option[Int]]) {

    var concept: String = ""
    var nodes: Node => List[List[(String, Node)]] = x => List()
    // function that returns the nodes
    var spanIndex: List[List[(String, Node)]] => List[Int] = x => List()
    // function that returns pointers to spans to update
    var unalignedOnly: Boolean = true

    var spans: (Node, List[Int]) => List[(Int, Int)] = (node, spanIndices) => {
      // default keeps the words unchanged
      spanIndices.map(x => (graph.spans(x).start, graph.spans(x).end))
    }

    def update(node: Node) {
      log(2, "SpanUpdater concept regex: " + concept)
      log(2, "SpanUpdater processing node: " + node.concept)
      if (node.concept.matches(concept) && (!node.isAligned(graph) || !unalignedOnly)) {
        log(2, "SpanUpdater matched concept regex: " + concept)
        val allNodes = nodes(node)
        log(2, "allNodes = " + allNodes.toString)
        val updateIndices = if (!allNodes.isEmpty) {
          spanIndex(allNodes)
        } else {
          List()
        }
        log(2, "updateIndices = " + updateIndices.toString)
        val wordSpans = if (!allNodes.isEmpty) {
          spans(node, updateIndices)
        } else {
          List()
        }
        //for ((spanIndex, (nodes, span)) <- updateIndices.zip(allNodes.zip(wordSpans))) {
        //    assert(spans == (graph.spans(spanIndex).start, graph.spans(spanIndex).end), "SpanUpdater does not support changing the word span")  // If you want to do this, you must call graph.updateSpan, AND fix up the wordToSpan map
        assert(allNodes.size == updateIndices.size && allNodes.size == wordSpans.size, "SpanUpdater: Number of spans do not match")
        for (((spanIndex, nodes), span) <- updateIndices.zip(allNodes).zip(wordSpans)) {
          if (nodes.size > 0) {
            log(2, "Calling updateSpan( span=" + spanIndex.toString + ", start=" + span._1 + ", end=" + span._2 + ", nodes=" + nodes.map(_._2.id).toString + " )")
            updateWordMap(spanIndex, (graph.spans(spanIndex).start, graph.spans(spanIndex).end), span, wordToSpan)
            graph.updateSpan(spanIndex, span._1, span._2, nodes.map(_._2.id), graph.spans(spanIndex).coRef, sentence)
            //log(2, "Calling updateSpan( "+spanIndex.toString+", "+nodes.map(_._2.id).toString+" )")
            //graph.updateSpan(spanIndex, nodes.map(_._2.id), sentence)
          } else {
            log(2, "Deleting span")
            graph.updateSpan(spanIndex, 0, 0, List(), false, sentence) // this effectively deletes the span
          }
        }
      }
    }
  }

  class ConceptAndChildren(override val sentence: Array[String],
                           override val graph: Graph
                          ) extends SpanAligner3(sentence, graph) {
    // To use specify children, and words
    var children: Map[String, String] = Map()

    tabSentence = "\t" + sentence.mkString("\t").toLowerCase + "\t"
    def checkMatch(input: (String, Node)): Boolean = {
      log(2, "Checking match on: " + input._1 + " " + getConcept(input._2.concept))
      var matches = true
      for ((label, concept) <- children) {
        log(2, "against " + label + " " + concept)
        if (input._1.matches(label) && !getConcept(input._2.concept).matches(concept)) {
          matches = false
        }
      }
      log(2, "returning " + matches)
      return matches
    }

    nodes = node => {
      log(2, "Filtered children = " + node.children.filter(x => checkMatch(x)))
      log(2, "children.size = " + children.size)
      log(2, "Check: " + (node.children.filter(x => checkMatch(x)) == children.size).toString)
      if (node.children.filter(x => checkMatch(x)).size == children.size) {
        // all specified children are there
        log(2, "Returning: " + (("", node) :: node.children.filter(x => checkMatch(x))).map(x => (x._1, getConcept(x._2.concept))))
        ("", node) :: node.children.filter(x => checkMatch(x))
      } else {
        log(2, "Check failed")
        List()
      }
    }
  }

  class UnalignedConcept(override val sentence: Array[String],
                         override val graph: Graph,
                         override val wordToSpan: Array[Option[Int]])
    extends SpanUpdater3(sentence, graph, wordToSpan) {

    var label: String = ""
    var num = 1000

    nodes = node => {
      if (node.children.exists(_._1.matches(label)) && node.children.count(_._1.matches(label)) <= num) {
        try {
          List(("", node) :: graph.spans(node.children.filter(_._1.matches(label))(0)._2.spans(0)).nodeIds.map(x => ("", graph.getNodeById(x))))
        } catch {
          case e: Throwable => List()
        }
      } else {
        List()
      }
    }
    spanIndex = nodes => nodes.map(x => x(1)._2.spans.filter(!graph.spans(_).coRef)(0)) // 2nd node of each span
    unalignedOnly = true
  }

  class UnalignedChild(override val sentence: Array[String],
                       override val graph: Graph,
                       override val wordToSpan: Array[Option[Int]])
    extends SpanUpdater3(sentence, graph, wordToSpan) {

    var label: String = ""
    var words: String = ".*"

    nodes = node => {
      if ((node.children.exists(_._1.matches(label))) && (node.children.filter(_._1.matches(label))(0)._2.spans.size == 0) && sentence.slice(graph.spans(node.spans(0)).start, graph.spans(node.spans(0)).end).mkString("\t").matches(words)) {
        try {
          val child: Node = node.children.filter(_._1.matches(label))(0)._2
          List(graph.spans(node.spans(0)).nodeIds.map(x => ("", graph.getNodeById(x))) ::: List(("ARG0-of", child)))
        } catch {
          case e: Throwable => List()
        }
      } else {
        List()
      }
    }
    spanIndex = nodes => nodes.map(x => x(0)._2.spans(0)) // 1nd node of each span
    unalignedOnly = false
  }

  private def toSpan(startEnd: (Int, Int), nodeIds: List[String], sentence: Array[String], graph: Graph, coRef: Boolean): Span = {
    // m is a match object which is a match in tabSentence (tabSentence = sentence.mkString('\t'))
    // note: tabSentence does not have to be sentence.mkString('\t') (it could be lowercased, for example)
    assert(nodeIds.size > 0, "Error: matchToSpan passed nodeIds with zero length")
    val (start, end) = startEnd
    val amr = SpanLoader.getAmr(nodeIds, graph)
    val words = sentence.slice(start, end).mkString(" ")
    val span = if (!coRef) {
      Span(start, end, nodeIds, words, amr, coRef)
    } else {
      val node = nodeIds(0)
      Span(start, end, List(node), words, SpanLoader.getAmr(List(node), graph), coRef)
    }
    return span
  }

  private def matchToIndices(m: Regex.Match, tabSentence: String): (Int, Int) = {
    log(2, "matchToIndices: m.start = " + m.start.toString)
    log(2, "matchToIndices: m.end = " + m.end.toString)
    log(2, "returning (" + (getIndex(tabSentence, m.start)).toString + ", " + (getIndex(tabSentence, m.end) - 1).toString + ")")
    return (getIndex(tabSentence, m.start), getIndex(tabSentence, m.end) - 1)
  }

  private def getIndex(tabSentence: String, charIndex: Int): Int = {
    log(2, "slice = [" + tabSentence.slice(0, charIndex).toString + "]")
    return tabSentence.view.slice(0, charIndex).count(_ == '\t') // views (p.552 stairway book)
  }

  private val ConceptExtractor = """^"?(.+?)-?[0-9]*"?$""".r // works except for numbers

  private def getConcept(conceptStr: String): String = {
    var ConceptExtractor(concept) = conceptStr
    if (conceptStr.matches("""^[0-9.]*$""")) {
      concept = conceptStr
    }
    return concept
  }

  private def updateWordMap(spanIndex: Int, oldSpan: (Int, Int), newSpan: (Int, Int), wordToSpan: Array[Option[Int]]) {
    for (i <- Range(oldSpan._1, oldSpan._2)) {
      assert(wordToSpan(i) == Some(spanIndex), "Error removing span from wordToSpan map")
      wordToSpan(i) = None
    }
    for (i <- Range(newSpan._1, newSpan._2)) {
      wordToSpan(i) = Some(spanIndex)
    }
  }


  private def isAligned(node: Node, graph: Graph): Boolean = {
    node.spans.exists(!graph.spans(_).coRef)
  }

  // TODO: move this into Node?
  private def getAlignment(node: Node, graph: Graph): Int = {
    node.spans.filter(!graph.spans(_).coRef)(0)
  }

  private def replaceAll(string: String, map: Map[String, String]): String = {
    (map :\ string) ((keyValue, s) => s.replaceAll(keyValue._1, keyValue._2))
  }

  /** **** This stuff was originally in Graph *******/

  private def overlap(span: Span, graph: Graph, wordToSpan: Array[Option[Int]]): Boolean = {
    var overlap = false
    for (id <- span.nodeIds) {
      overlap = (graph.getNodeById(id).spans.map(x => !graph.spans(x).coRef) :\ overlap) (_ || _)
    }
    for (word <- Range(span.start, span.end)) {
      if (wordToSpan(word).isDefined) {
        // TODO: what about corefs
        overlap = true
      }
    }
    overlap
  }
  /** **** </This stuff was originally in Graph> *******/
  //private val conceptRegex = """-[0-9]+$""".r
  //private val ConceptExtractor = "([a-zA-Z0-9.-]+ *)|\"([^\"]+)\" *".r
  //private val ConceptExtractor = """([a-zA-Z0-9.-]+)\|(?:"([^ ]+)")""".r
  //private val ConceptExtractor = """"?([a-zA-Z0-9.-]+)"?""".r
  //private val ConceptExtractor = """^"?(.+?)(?:-[0-9]+)"?$""".r
  //private val ConceptExtractor = """^"?(.+?)-?[0-9]*"?$""".r // works except for numbers
  def alignWord(stemmedSentence: Array[List[String]], node: Node, alignments: Array[Option[Int]]): Option[Int] = {
    log(3, "alignWords: node.concept = " + node.concept)
    var concept = getConcept(node.concept)
    var alignment: Option[Int] = None
    for (i <- stemmedSentence.indices) {
      for (word <- stemmedSentence(i)) {
        if (word == concept && alignments(i).isEmpty) {
          if (alignment.isDefined) {
            log(1, "WARNING: Found duplicate match for concept " + node.concept)
          } else {
            log(3, "concept: " + node.concept + " word: " + word)
            alignment = Some(i)
          }
        }
      }
    }
    alignment
  }

  def fuzzyMatchLength(stemmedSentence: Array[List[String]], node: Node): Int = {
    var concept = getConcept(node.concept)
    val size = stemmedSentence.length
    val matchlength = new Array[Int](size)
    for (i <- Range(0, size)) {
      matchlength(i) = 0
      for (word <- stemmedSentence(i)) {
        val len = matchLength(word, concept)
        if (len > matchlength(i)) {
          matchlength(i) = len
        }
      }
    }
    matchlength.max
  }

  def fuzzyAlign(stemmedSentence: Array[List[String]], node: Node, alignments: Array[Option[Int]]): Option[Int] = {
    var concept = getConcept(node.concept)
    val size = stemmedSentence.length
    var alignment: Option[Int] = None
    val matchlength = new Array[Int](size)
    for (i <- Range(0, size)) {
      matchlength(i) = 0
      for (word <- stemmedSentence(i)) {
        val len = matchLength(word, concept)
        if (len > matchlength(i)) {
          matchlength(i) = len
        }
      }
    }
    val max = matchlength.max
    if (max >= 4) {
      for (i <- Range(0, size) if matchlength(i) == max && alignments(i).isEmpty) {
        if (alignment.isEmpty) {
          log(1, "Fuzzy Matcher concept: " + node.concept + " word: " + stemmedSentence(i).head)
          alignment = Some(i)
        } else {
          log(1, "WARNING: duplicate fuzzy matches for concept " + node.concept)
        }
      }
    }
    alignment
  }

  def matchLength(string1: String, string2: String): Int = {
    var length = 0
    for (i <- Range(0, min(string1.length, string2.length))) {
      if (string1(i) == string2(i) && length == i) {
        length = i + 1
      }
    }
    length
  }

  def stemmer(word: String): List[String] = {
    val stems = Wordnet.stemmer(word)
    val numbers = word.toLowerCase match {
      case "one" => List("1")
      case "two" => List("2")
      case "three" => List("3")
      case "four" => List("4")
      case "five" => List("5")
      case "six" => List("6")
      case "seven" => List("7")
      case "eight" => List("8")
      case "nine" => List("9")
      case _ => List()
    }
    val months = word match {
      case "January" => List("1")
      case "February" => List("2")
      case "March" => List("3")
      case "April" => List("4")
      case "May" => List("5")
      case "June" => List("6")
      case "July" => List("7")
      case "August" => List("8")
      case "September" => List("9")
      case "October" => List("10")
      case "November" => List("11")
      case "December" => List("12")
      case _ => List()
    }
    var exceptions = word.toLowerCase match {
      case "/" => List("slash")
      case ";" => List("and")
      case ":" => List("mean")
      case "!" => List("expressive")
      case ".." => List("expressive")
      case "..." => List("expressive")
      case "...." => List("expressive")
      case "?" => List("interrogative")
      case "%" => List("percentage-entity")
      case "able" => List("possible")
      case "according" => List("say")
      case "also" => List("include")
      case "anti" => List("oppose","counter")
      case "anyway" => List("have-concession")
      case "as" => List("same")
      case "because" => List("cause")
      //case "biggest" => List("most")  //anything est
      case "but" => List("contrast","have-concession")
      case "can" => List("possible")
      case "cant" => List("possible")
      case "can't" => List("possible")
      case "choice" => List("choose")
      case "could" => List("possible")
      case "death" => List("die")
      case "French" => List("france","France")
      case "french" => List("france","France")
      case "have" => List("obligate") // as in "have to submit the assignment"  maybe later in pipeline?
      case "her" => List("she")
      case "his" => List("he")
      case "how" => List("amr-unknown")
      case "if" => List("cause")
      case "illegal" => List("law")
      case "like" => List("resemble")
      case "life" => List("live")
      case "may" => List("possible")
      case "me" => List("i")
      case "might" => List("possible")
      case "my" => List("i")
      case "n't" => List("-")
      case "no" => List("-")
      case "non" => List("-")
      case "not" => List("-")
      case "of" => List("include","have-manner")
      case "ok" => List("okay")
      case "o.k." => List("okay")
      case "our" => List("we")
      case "people" => List("person")
      case "similar" => List("resemble")
      case "since" => List("cause")
      case "should" => List("recommend")
      case "so" => List("infer","cause")  // maybe cause should come later in pipeline
      case "speech" => List("speak")
      case "statement" => List("state")
      case "them" => List("they")
      case "these" => List("this")
      case "those" => List("that") // also "person", see DF-225-195986-849_2659.9
      case "thoughts" => List("think")
      case "thoughs" => List("think")
      case "uni" => List("university")
      case "well" => List("good")
      case "what" => List("amr-unknown")  // also "thing" sometimes
      case "who" => List("amr-unknown")  // also "thing" sometimes
      //case "yet" => List("have-concession")  // should come later in pipeline
      case _ => List()
    }

    if (word.toLowerCase.matches("""(in|un|ir).*""")) {
      exceptions = word.drop(2) :: exceptions  // should include "-"
    }
    if (word.toLowerCase.matches(""".*er""")) {
      exceptions = word.dropRight(2) :: exceptions  // should include "-"
    }
    if (word.toLowerCase.matches(""".*ers""")) {
      exceptions = word.dropRight(3) :: exceptions  // should include "-"
    }
    //if (word.matches("""^[0-9]*$""")) {
    //    numbers = word.toInt.toString :: numbers
    //}
    (word :: word.toLowerCase :: numbers ::: months ::: exceptions ::: stems).distinct
  }

  def logUnalignedConcepts(node: Node) {
    if (node.alignment.isEmpty) {
      log(1, "WARNING: Unaligned concept " + node.concept)
    }
    for ((_, child) <- node.topologicalOrdering) {
      logUnalignedConcepts(child)
    }
  }

}

