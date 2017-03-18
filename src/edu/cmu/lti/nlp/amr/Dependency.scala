package edu.cmu.lti.nlp.amr

import scala.util.matching.Regex
import scala.collection.mutable.Map
import scala.collection.mutable.Set
import scala.collection.mutable.ArrayBuffer

case class Dependency(head: Int, dependent: Int, relation: String)

object Dependency {

  private val Stanford = """([^(]+[^-]+([0-9]+), *[^-]+([0-9]+)) *""".r

  def fromConll(string: String): Dependency = {
    val fields = string.split("\t")
    Dependency(fields(6).toInt - 1, fields(0).toInt - 1, fields(7))
  }

  def fromStanford(string: String): Dependency = {
    val Stanford(relation, head, dependent) = string
    Dependency(head.toInt - 1, dependent.toInt - 1, relation)
  }
}

