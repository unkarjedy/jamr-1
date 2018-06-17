package scripts.smatch

import java.io.{File, PrintStream}

import edu.cmu.lti.nlp.amr.utils.{CorpusUtils, DepsTextBlock}
import org.slf4j.{Logger, LoggerFactory}
import scripts.utils.FileExt._

import scala.io.Source
import scala.sys.process.ProcessLogger

// given input file with a list of AMRs with tags, parses sntId of the AMRs and extracts
// corresponding AMRs from another file with AMRS saving them to second file
// TODO: THE SHITTIEST COMMENT EVER!
object PrepareSmatchFileTuplesAndRun {
  private val logger: Logger = LoggerFactory.getLogger(getClass.getSimpleName)

  private val jamrRoot = "C:/Users/unkarjedy/Desktop/Diploma/Jamr_Fork/"
  private val smatchScriptPath = s"$jamrRoot/scripts/smatch_v1_0/smatch_modified.py"
  private val smatchPlaygroundDir =
    s"$jamrRoot/resources_terms/FinalOutputs/" +
      s"smatch_evals_battlefield/sent2_without_features/"

  private val fileNameBase = "sentences2.txt"
  //  private val fileNameBase = "sentences1_fixed1.txt"

  private lazy val targetFolder = {
    val f = new File(smatchPlaygroundDir).resolve("target")
    f.mkdirs()
    f
  }

  private val amrFileSuffixGold = "amr_gold"
  private val amrFileSuffixesTest = Seq(
    "amr_deps_gold",
    "amr_deps_0",
    "amr_deps_1",
    "amr_deps_2",
    "amr_deps_3",
    "amr_deps_4"
  )

  private val skipWithDepsUnchanged: Boolean = true
  private val suffixGoldDeps: String = "deps.gold.txt"
  private val suffixStanfDeps: String = "deps.0_best.txt"

  def main(args: Array[String]): Unit = {
    logger.info(s"Golden file suffix: $amrFileSuffixGold")
    logger.info(s"Test file suffixes: ${amrFileSuffixesTest.mkString(", ")}")

    val amrBlocksGold: Seq[AmrBlock] = extractAmrBlocksForFileSuffix(amrFileSuffixGold)
    val skipSntIds: Seq[String] =
      if (!skipWithDepsUnchanged) Seq()
      else calculateSntIdsToSkip(amrBlocksGold).map(_.toString)

    prepareSmatchFileTuples(amrBlocksGold, skipSntIds)
    runSmatchEvaluations(skipSntIds)
  }

  private def prepareSmatchFileTuples(amrBlocksGold: Seq[AmrBlock], skipSntIds: Seq[String]): Unit = {

    if (skipSntIds.nonEmpty) {
      logger.info("")
      logger.info(s"Ignoring  ${skipSntIds.size} sentences during Smatch Evaluations.")
      logger.info(s"Dependency trees for those sentences were not changed during manual fixes.")
      logger.info(s"# ::sntId: ${skipSntIds.mkString("(", ",", ")")}")
      logger.info("")
    }

    amrFileSuffixesTest.foreach { suffix =>
      val amrBlocksTest = extractAmrBlocksForFileSuffix(suffix)

      val targetSubFolder = targetFolder.resolve(fileTupleFolderName(amrFileSuffixGold, suffix))
      targetSubFolder.mkdirs()

      lazy val outGold = new PrintStream(targetSubFolder.resolve(s"$amrFileSuffixGold.smatch"))
      lazy val outTest = new PrintStream(targetSubFolder.resolve(s"$suffix.smatch"))
      lazy val sntIdToBlockTest = amrBlocksTest.groupBy(_.sntId).mapValues(_.head)

      logger.info(s"Preparing $suffix vs $amrFileSuffixGold")
      amrBlocksGold
        .filterNot(block => skipSntIds.contains(block.sntId))
        .foreach { block1 =>
          sntIdToBlockTest.get(block1.sntId) match {
            case Some(block2) =>
              outGold.println(block1.text)
              outGold.println()
              outTest.println(block2.text)
              outTest.println()
            case None =>
              logger.warn(s"    no corresponding AMR for sentence: ${block1.sntId}")
          }
        }
    }
  }


  /**
    * This returns ids of the sentences that we do not want to take part during F1 score calculating.
    * Those are basically the sentences for which we did not fix dependency trees.
    */
  def calculateSntIdsToSkip(amrBlocksGold: Seq[AmrBlock]): Seq[Int] = {
    val depBlocksGold: Array[DepsTextBlock] = CorpusUtils.getDepsBlocks(getFileNameForSuffix(suffixGoldDeps)).toArray
    val depBlocksStanf: Array[DepsTextBlock] = CorpusUtils.getDepsBlocks(getFileNameForSuffix(suffixStanfDeps)).toArray

    val idToStanfText: Map[Int, String] = depBlocksStanf.groupBy(_.sntId.get).mapValues(_.head.conllText)

    depBlocksGold
      .filterNot { block =>
        idToStanfText.getOrElse(block.sntId.get, "") != block.conllText
      }
      .map(_.sntId.get)
      .distinct
  }

  def fileTupleFolderName(suffix1: String, suffix2: String) = {
    suffix1 + "_VS_" + suffix2
  }

  private def extractAmrBlocksForFileSuffix(suffix: String): Seq[AmrBlock] =
    extractAmrBlocks(getFileNameForSuffix(suffix))

  private def getFileNameForSuffix(suffix: String): File =
    new File(smatchPlaygroundDir + fileNameBase + "." + suffix)

  private def extractAmrBlocks(file: File): Seq[AmrBlock] = {
    val textBlocks: Iterator[String] = CorpusUtils.splitFileOnNewline(file)
    val amrBlocks: Iterator[AmrBlock] =
      textBlocks
        .filter(_.lines.exists(_.trim.startsWith("(")))
        .map { block =>
          val lines = block.lines
          val sntIdReg = """#\s+::sntId\s+(.*)""".r
          val sntIdCandidates = lines.collect { case sntIdReg(id) => id }
          val sntId = sntIdCandidates.toList match {
            case id :: Nil => id.toString
            case _ => throw new IllegalArgumentException(s"Can't find sntId tag in AMR block:\n$block")
          }
          AmrBlock(sntId, block)
        }
    amrBlocks.toArray.toSeq
  }

  private def runSmatchEvaluations(skipSntIds: Seq[String]): Unit = {
    val ps = new PrintStream(targetFolder.resolve("RESULTS.txt"))

    ps.println(s"Smatch evaluations")
    val goldAmrsFile: File = getFileNameForSuffix(amrFileSuffixGold)
    ps.println(s"Gold AMRs file: ${goldAmrsFile.getName}")
    val sntIdPrefix = "# ::sntId "
    val sntIds: Array[Int] = collectSntIDs(goldAmrsFile, sntIdPrefix)
    ps.println(s"# ::sntIds ${sntIds.min}-${sntIds.max}")
    ps.println(s"# ::sntIds skipped (${skipSntIds.size}): ${skipSntIds.mkString("(", ",", ")")}")

    amrFileSuffixesTest.foreach { testSuffix =>
      val targetSubFolder = targetFolder.resolve(fileTupleFolderName(amrFileSuffixGold, testSuffix))
      val command = smatchCommand(
        testedFile = targetSubFolder.resolve(s"$testSuffix.smatch").toString,
        goldFile = targetSubFolder.resolve(s"$amrFileSuffixGold.smatch").toString
      )

      logger.info(s"Running $testSuffix vs $amrFileSuffixGold.")
      ps.println()
      ps.println(testSuffix + " VS " + amrFileSuffixGold)
      val errorCode = sys.process.stringToProcess(command) ! ProcessLogger(
        out => {
          ps.println(out)
          logger.info(out)
        },
        err => {
          logger.error(err)
        }
      )
      if (errorCode != 0) {
        throw new RuntimeException(s"Smatch script exited with error code $errorCode")
      }
    }
  }

  private def collectSntIDs(goldAmrsFile: File, sntIdPrefix: String) = {
    Source.fromFile(goldAmrsFile).getLines()
      .filter(_.startsWith(sntIdPrefix))
      .map(line => line.substring(sntIdPrefix.length).trim.toInt)
      .toArray
  }
  private def smatchCommand(testedFile: String, goldFile: String): String = {
    s"""python $smatchScriptPath --pr -f "$testedFile" "$goldFile""""
  }

  private case class AmrBlock(sntId: String, text: String)
}
