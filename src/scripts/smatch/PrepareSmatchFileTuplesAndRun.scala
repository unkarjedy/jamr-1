package scripts.smatch

import java.io.{File, PrintStream}

import edu.cmu.lti.nlp.amr.utils.CorpusUtils
import org.slf4j.{Logger, LoggerFactory}
import scripts.utils.FileExt._

import scala.sys.process.ProcessLogger

// given input file with a list of AMRs with tags, parses sntId of the AMRs and extracts
// corresponding AMRs from another file with AMRS saving them to second file
// TODO: THE SHITTIEST COMMENT EVER!
object PrepareSmatchFileTuplesAndRun {
   val logger: Logger = LoggerFactory.getLogger(getClass.getSimpleName)

  private val jamrRoot = "C:/Users/unkarjedy/Desktop/Diploma/Jamr_Fork/"
  val smatchScriptPath = s"$jamrRoot/scripts/smatch_v1_0/smatch_modified.py"
  private val smatchPlaygroundDir = s"$jamrRoot/resources_terms/FinalOutputs/smatch_evals_battlefield/"

  private val fileNameBase = "most_used_term_defs_manual_FIXED1.txt"

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

  private def extractAmrBlocksForSuffix(suffix: String): Seq[AmrBlock] =
    extractAmrBlocks(getFileNameForSuffix(suffix))

  private def getFileNameForSuffix(suffix: String) =
    new File(smatchPlaygroundDir + fileNameBase + "." + suffix)

  def main(args: Array[String]): Unit = {
    logger.info(s"Golden file suffix: $amrFileSuffixGold")
    logger.info(s"Test file suffixes: ${amrFileSuffixesTest.mkString(", ")}")
    prepareSmatchFileTuples()
    runSmatchEvaluations()
  }

  private def prepareSmatchFileTuples(): Unit = {
    val amrBlocksGold: Seq[AmrBlock] = extractAmrBlocksForSuffix(amrFileSuffixGold)

    amrFileSuffixesTest.foreach { suffix =>
      val amrBlocksTest = extractAmrBlocksForSuffix(suffix)

      val targetSubFolder = targetFolder.resolve(fileTupleFolderName(amrFileSuffixGold, suffix))
      targetSubFolder.mkdirs()

      lazy val outGold = new PrintStream(targetSubFolder.resolve(s"$amrFileSuffixGold.smatch"))
      lazy val outTest = new PrintStream(targetSubFolder.resolve(s"$suffix.smatch"))
      lazy val idToBlockTest = amrBlocksTest.groupBy(_.sntId).mapValues(_.head)
      logger.info(s"Preparing $suffix vs $amrFileSuffixGold")
      amrBlocksGold.foreach { block1 =>
        idToBlockTest.get(block1.sntId) match {
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

  private def fileTupleFolderName(suffix1: String, suffix2: String) = {
    suffix1 + "_VS_" + suffix2
  }

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

  private def runSmatchEvaluations(): Unit = {
    val resultsPrintStream = new PrintStream(targetFolder.resolve("RESULTS.txt"))
    resultsPrintStream.println(s"Smatch evaluations")
    resultsPrintStream.println(s"Gold AMRs file: ${getFileNameForSuffix(amrFileSuffixGold).getName}")

    amrFileSuffixesTest.foreach { testSuffix =>
      val targetSubFolder = targetFolder.resolve(fileTupleFolderName(amrFileSuffixGold, testSuffix))
      val command = smatchCommand(
        testedFile = targetSubFolder.resolve(s"$testSuffix.smatch").toString,
        goldFile = targetSubFolder.resolve(s"$amrFileSuffixGold.smatch").toString
      )

      logger.info(s"Running $testSuffix vs $amrFileSuffixGold. Trees count")
      resultsPrintStream.println()
      resultsPrintStream.println(testSuffix + " VS " + amrFileSuffixGold)
      val errorCode = sys.process.stringToProcess(command) ! ProcessLogger(
        out => {
          resultsPrintStream.println(out)
          logger.info(out)
        },
        err => {
          logger.error(err)
        }
      )
      if(errorCode != 0) {
        throw new RuntimeException(s"Smatch script exited with error code $errorCode")
      }
    }
  }

  private def smatchCommand(testedFile: String, goldFile: String): String = {
    s"""python $smatchScriptPath --pr -f "$testedFile" "$goldFile""""
  }

  private case class AmrBlock(sntId: String, text: String)
}
