package edu.cmu.lti.nlp.amr.span

import edu.cmu.lti.nlp.amr.graph.Node

case class Span(var start: Int,
                var end: Int,
                var nodeIds: List[String],
                var words: String,
                var amrNode: Node,
                var coRef: Boolean) {
  def format(): String = {
    val common = s"${start.toString}-${end.toString}|${nodeIds.mkString("+")}"

    if (start < end) {
      if (coRef)
        s"*$common"
      else
        common
    } else {
      ""
    }
  }
}

object Span {
  // NAUMENKO
  // I do not want to make a method equals not to brake all the existing code somewhere...
  def equalsTo(span1: Span)(span2: Span): Boolean = {
    equalBorders(span1)(span2) &&
      span1.amrNode.toString == span2.amrNode.toString
  }

  def equalBorders(span1: Span)(span2: Span): Boolean = {
    span1.start == span2.start && span1.end == span2.end
  }
}


