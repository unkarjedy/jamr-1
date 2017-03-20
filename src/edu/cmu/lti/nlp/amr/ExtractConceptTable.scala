package edu.cmu.lti.nlp.amr

import java.io.{InputStream, PrintStream}

import edu.cmu.lti.nlp.amr.BasicFeatureVector._
import edu.cmu.lti.nlp.amr.ConceptInvoke.PhraseConceptPair
import edu.cmu.lti.nlp.amr.ExtractConceptTable.OptionMap

import scala.collection.{immutable => i, mutable => m}

object ExtractConceptTable {

  val usage = """Usage: scala -classpath . edu.cmu.lti.nlp.amr.ExtractConceptTable < aligned_amr_corpus > concept_table"""
  type OptionMap = m.Map[Symbol, String]

  val implementedFeatures: m.Set[String] = m.Set("corpusIndicator", "corpusLength", "count", "conceptGivenPhrase", "phraseGivenConcept")

  def parseOptions(map: OptionMap, list: List[String]): OptionMap = {
    def isSwitch(s: String) = s(0) == '-'
    list match {
      case Nil => map
      case "--stage1-features" :: value :: l => parseOptions(map + ('stage1Features -> value), l)
      case "--max-training-instances" :: value :: l => parseOptions(map + ('maxTrainingInstances -> value), l)
      case "-v" :: value :: l => parseOptions(map + ('verbosity -> value), l)
      case option :: tail => System.err.println("Error: Unknown option " + option)
        sys.exit(1)
    }
  }

  def main(args: Array[String]): Unit = {
    val options = parseOptions(m.Map(), args.toList)
    if (options.contains('verbosity)) {
      verbosityGlobal = options('verbosity).toInt
    }
    if (!options.contains('stage1Features)) {
      System.err.println("Error: no stage1 features specified during concept table extraction")
      sys.exit(1)
    }

    val stage1Features = options('stage1Features).split(",").toList
    val maxCount = options.getOrElse('maxTrainingInstances, "10000").toInt

    val runner = new ExtractConceptTable(stage1Features, maxCount,
                                         System.in, System.out, System.err)
    runner.run()
  }
}

class ExtractConceptTable(stage1Features: List[String],
                          maxTrainingInstances: Int,
                          in: InputStream,
                          out: PrintStream,
                          err: PrintStream) extends Runnable {

  override def run(): Unit = {
    logger(0, "stage1Features = " + stage1Features)

    val inputLines = Corpus.splitOnNewline(Source.fromInputStream(in).getLines).toArray
    val conceptTable: m.Map[String, List[PhraseConceptPair]] = extract(inputLines, stage1Features, maxTrainingInstances)

    for {(_, list) <- conceptTable
         concept <- list} {
      out.println(concept)
    }
  }

  // Collect all phrase concept pairs
  def extract(corpusLines: Array[String],
              featureNames: List[String],
              maxCount: Int): m.Map[String, List[PhraseConceptPair]] = {

    val phraseConceptPairs: m.Map[(List[String], String), (PhraseConceptPair, Int, Int)] = m.Map()
    val tokenized: Array[Array[String]] = corpusLines.map(x => Array.empty[String])
    val fragmentCounts: m.Map[String, Int] = m.Map()
    val singleConceptCounts: m.Map[String, Int] = m.Map()
    var totalConcepts = 0.0
    var idx = 0

    corpusLines
      .filter(CorpusTool.doesContainSomeAmr)
      .map(AMRTrainingData.apply)
      .foreach(block => {
        tokenized(idx) = block.sentence
        block.loadSpans()
        for (span <- block.graph.spans) {
          val amr = span.amrNode.toString().replaceAll(""":op[0-9]*""", ":op")
          fragmentCounts(amr) = fragmentCounts.getOrElse(amr, 0) + 1
          for (concept <- span.nodeIds.map(id => block.graph.getNodeById(id).concept)) {
            singleConceptCounts(concept) = singleConceptCounts.getOrElse(concept, 0) + 1
            totalConcepts += 1.0
          }
          val words = span.words.split(" ").toList
          val key = (words, amr)
          if (phraseConceptPairs.contains(key)) {
            // Could extract more features here, and add them to existing features from the pair
            var (phraseConceptPair, phraseConceptPairCount, trainingInstanceCount) = phraseConceptPairs(key)
            var trainingIndices = phraseConceptPair.trainingIndices
            if (trainingIndices.head != idx) {
              trainingIndices = idx :: trainingIndices // don't add training example twice
              trainingInstanceCount += 1 // this # is wrong if > maxCount, but in that case we don't care
            }
            if (trainingInstanceCount > maxCount) {
              trainingIndices = List()
            }
            phraseConceptPairCount += 1
            phraseConceptPairs(key) = (PhraseConceptPair(words, amr, FeatureVector(), trainingIndices), phraseConceptPairCount, trainingInstanceCount)
          } else {
            // Could extract more features here
            phraseConceptPairs(key) = (PhraseConceptPair(words, amr, FeatureVector(), List(idx)), 1, 1)
          }
        }
        idx += 1
      })

    // Make the map from words to phraseConceptPairs (conceptTable), and initialize phraseCounts to zero
    val phraseCounts: m.Map[List[String], Int] = m.Map()
    val phraseConceptPairTable: m.Map[String, List[PhraseConceptPair]] = m.Map() // map from first word in the phrase to list of PhraseConceptPair
    for ((_, (phraseConceptPair, _, _)) <- phraseConceptPairs) {
      val phraseFirstWord = phraseConceptPair.words.head
      phraseConceptPairTable(phraseFirstWord) = phraseConceptPair :: phraseConceptPairTable.getOrElse(phraseFirstWord, List())
      phraseCounts(phraseConceptPair.words) = 0
    }

    // Count the phrases in the corpus
    for (sentence <- tokenized) {
      for ((word, i) <- sentence.zipWithIndex) {
        val matching = phraseConceptPairTable.getOrElse(word, List()).filter(x => x.words == sentence.slice(i, i + x.words.size).toList)
        for (phraseConceptPair <- matching) {
          phraseCounts(phraseConceptPair.words) = phraseCounts(phraseConceptPair.words) + 1
        }
      }
    }

    // Update the conceptTable features
    for {(_, list) <- phraseConceptPairTable
         phraseConceptPair <- list} {
      val count = phraseConceptPairs((phraseConceptPair.words, phraseConceptPair.graphFrag))._2.toDouble
      if (featureNames.contains("corpusIndicator")) {
        phraseConceptPair.features += FeatureVector(m.Map("corpus" -> 1.0))
      }
      if (featureNames.contains("corpusLength")) {
        phraseConceptPair.features += FeatureVector(m.Map("corpus_len" -> phraseConceptPair.words.size))
      }
      if (featureNames.contains("count")) {
        phraseConceptPair.features += FeatureVector(m.Map("N" -> count))
      }
      if (featureNames.contains("logPrConcept")) {
        for (node <- phraseConceptPair.graph.nodes) {
          phraseConceptPair.features += FeatureVector(m.Map("logPrConcept" -> log(singleConceptCounts(node.concept) / totalConcepts)))
        }
      }
      if (featureNames.contains("conceptGivenPhrase")) {
        phraseConceptPair.features += FeatureVector(m.Map("c|p" -> log(count / phraseCounts(phraseConceptPair.words).toDouble)))
      }
      if (featureNames.contains("phraseGivenConcept")) {
        phraseConceptPair.features += FeatureVector(m.Map("p|c" -> log(count / fragmentCounts(phraseConceptPair.graphFrag).toDouble)))
      }
    }

    phraseConceptPairTable
  }
}

