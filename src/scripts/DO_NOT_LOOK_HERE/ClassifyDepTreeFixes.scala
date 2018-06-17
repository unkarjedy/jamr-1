package scripts.DO_NOT_LOOK_HERE

import java.io.{File, PrintStream, PrintWriter}

import edu.cmu.lti.nlp.amr.utils.{CorpusUtils, DepsTextBlock}
import scripts.train.RunProperties
import scripts.utils.FileExt._

object ClassifyDepTreeFixes {

  def main(args: Array[String]): Unit = {
    val runProperties = new RunProperties("run.properties")

    val jamrRoot: File = new File(runProperties.jamrRoot)
    val finalPartsFolder: File = jamrRoot.resolve("resources_terms/FinalOutputs/gold_dep")
    val depTreesFolder: File = finalPartsFolder //.resolve("PART1/DEP_TREES")
    val baseFileName = "sentences2.txt"
    // val baseFileName = "sentences1_fixed1.txt"
    val depTreesGold: File = depTreesFolder.resolve(baseFileName + ".deps.gold.txt")
    val depTreesKBest: File = depTreesFolder.resolve(baseFileName + ".deps.0_best.txt")
    val outFolder: File = depTreesFolder.resolve("fixes_info")
    outFolder.mkdirs()
    val depTreesGoldWithStat: File = outFolder.resolve(baseFileName + ".gold_with_stat.txt")
    val statFile: File = outFolder.resolve(baseFileName + ".error_stat.txt")

    val errorStat: ErrorsStat =
      analize(depTreesGold, depTreesKBest, depTreesGoldWithStat)
    printErrorStats(statFile, errorStat)
  }

  private def printErrorStats(errorStat: File, stats: ErrorsStat) = {
    val ps = new PrintStream(errorStat)
    ps.println(s"REF: ${stats.ref}")
    ps.println(s"POS: ${stats.pos.size}")
    groupAndCount(stats.pos).foreach { case (err@(old, gold), count) =>
      ps.println(f"    $old%3s -> $gold%3s : $count%d")
    }
    ps.println(s"REL: ${stats.rel.size}")
    groupAndCount(stats.rel).foreach { case (err@(old, gold), count) =>
      ps.println(f"    $old%-5s -> $gold%-5s : $count%d")
    }
  }

  private def groupAndCount[T](seq: Seq[T]): Seq[(T, Int)] = {
    seq.groupBy(identity).mapValues(_.size).toSeq.sortBy(- _._2)
  }

  private def analize(depTreesGold: File, depTreesKBest: File, resultFile: File): ErrorsStat = {
    val sntIdToGold: Map[Int, DepsTextBlock] = CorpusUtils.getDepsBlocks(depTreesGold).toArray.groupBy(_.sntId.get).mapValues(_.head)
    val sntIdToKBest: Map[Int, Seq[DepsTextBlock]] = CorpusUtils.getDepsBlocks(depTreesKBest).toArray.groupBy(_.sntId.get).mapValues(_.sortBy(_.treeId.get))

    val printWriter = new PrintWriter(resultFile)
    var counter = 0
    val errorsStatSeq: Seq[ErrorsStat] =
      (for {
        sntId <- sntIdToGold.keySet.toSeq.sorted
      } yield {
        sntIdToKBest.get(sntId) match {
          case Some(blocksKBest) =>
            println(sntId)
            val blockGold = sntIdToGold(sntId)
            counter += 1
            Some(analizeForElement(printWriter, blockGold, blocksKBest))
          case None =>
            println(s"    missing sntId in kBest: $sntId")
            None
        }
      }) flatten

    println(s"Total entries: $counter")
    printWriter.close()

    val errorStatTotal: ErrorsStat = errorsStatSeq.foldLeft(ErrorsStat.empty)(_.combine(_))
    errorStatTotal
  }

  private def analizeForElement(printWriter: PrintWriter,
                                blockGold: DepsTextBlock,
                                blocksKBestAll: Seq[DepsTextBlock]): ErrorsStat = {
    val blockKBest = blocksKBestAll(0)
    val errorStat: ErrorsStat =
      blockGold.conllLines.zip(blockKBest.conllLines)
        .zipWithIndex
        .foldLeft(ErrorsStat(Seq(), Seq(), 0)) { case (stat, ((lineGold, lineStanf), idx)) =>
          val conllGold: ConllLine = ConllLine.parse(lineGold)
          val conllStanf: ConllLine = ConllLine.parse(lineStanf)

          val posErr: Option[(String, String)] =
            if (conllStanf.pos != conllGold.pos) Some(conllStanf.pos.get -> conllGold.pos.get)
            else None
          val relErr: Option[(String, String)] =
            if (conllStanf.deprel != conllGold.deprel) Some(conllStanf.deprel.get -> conllGold.deprel.get)
            else None
          val refErr: Int =
            if (conllStanf.gov != conllGold.gov) 1
            else 0

          stat.copy(
            stat.pos ++ posErr,
            stat.rel ++ relErr,
            stat.ref + refErr
          )
        }

    val blockUpdated = blockGold
      .withTag("err_pos", errorStat.pos.size + s": ${errorStat.pos.mkString(", ")}")
      .withTag("err_rel", errorStat.rel.size + s": ${errorStat.rel.mkString(", ")}")
      .withTag("err_ref", errorStat.ref.toString)
    printWriter.println(blockUpdated.mkString)
    printWriter.println()

    errorStat
  }

}

case class ErrorsStat(pos: Seq[(String, String)],
                      rel: Seq[(String, String)],
                      ref: Int) {
  def combine(other: ErrorsStat) = ErrorsStat(
    pos ++ other.pos,
    rel ++ other.rel,
    ref + other.ref
  )
}

object ErrorsStat {
  def empty = ErrorsStat(Seq(), Seq(), 0)
}

case class ConllLine(index: Option[Int],
                     form: Option[String],
                     lemma: Option[String],
                     pos: Option[String],
                     cpos: Option[String],
                     feats: Option[String],
                     gov: Option[Int],
                     deprel: Option[String],
                     phead: Option[Int],
                     pdeprel: Option[String]) extends Iterable[Option[_]] {
  override def toString(): String = {
    this.map(_.getOrElse("_")).mkString("\t")
  }

  override def iterator: Iterator[Option[_]] = {
    productIterator.asInstanceOf[Iterator[Option[_]]]
  }
}

object ConllLine {
  def parse(line: String): ConllLine = {
    val parts: Array[String] = line.split("\t")

    def get(idx: Int): Option[String] = {
      if (idx > parts.length - 1) {
        val debug = 2 + 2
      }
      parts(idx) match {
        case "_" => None
        case value => Some(value)
      }
    }

    ConllLine(
      index = get(0).map(_.toInt),
      form = get(1),
      lemma = get(2),
      pos = get(3),
      cpos = get(4),
      feats = get(5),
      gov = get(6).map(_.toInt),
      deprel = get(7),
      phead = get(8).map(_.toInt),
      pdeprel = get(9)
    )
  }

  def main(args: Array[String]): Unit = {
    test()
  }

  private def test(): Unit = {
    assertResult(
      ConllLine.parse("15\tthe\t_\tDT\tDT\t_\t16\tdet\t_\t_"),
      ConllLine(
        index = Some(15),
        form = Some("the"),
        lemma = None,
        pos = Some("DT"),
        cpos = Some("DT"),
        feats = None,
        gov = Some(16),
        deprel = Some("det"),
        phead = None,
        pdeprel = None
      )
    )
  }

  private def assertResult(result: Any, expected: Any) = {
    if (result == expected) {
      println("OK")
    } else {
      println("ERROR")
      println(s"EXPECTED  : $expected")
      println(s"RESULT    : $result")
      (expected, result) match {
        case (s1: String, s2: String) => println(s"DIFF: ${s1.diff(s2)}")
        case _ =>
      }
      throw new RuntimeException("Test failed")
    }
  }
}