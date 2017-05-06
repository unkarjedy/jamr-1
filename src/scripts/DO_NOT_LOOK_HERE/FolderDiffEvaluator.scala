package scripts.DO_NOT_LOOK_HERE

import java.io.File
import java.util.logging.FileHandler

import scripts.utils.logger.SimpleLoggerLike

import scala.collection.mutable
import scala.io.Source

/**
  * !!!! SKIP THIS FILE, DO NOT SPEND YOUR TIME !!!!
  * It is not directly used for JAMR purposes.
  * It is used to check if files content of two folders are equal.
  * I used it to check that Scala scripts outputs are same as shell scripts outputs.
  */
object FolderDiffEvaluator extends SimpleLoggerLike {

  def main(args: Array[String]): Unit = {
    val basePath = "C:/Users/unkarjedy/Desktop/NLP/jamr-master/data/"
    val folder1 = new File(s"$basePath/AMR-Bank-v1.6")
    val folder2 = new File(s"$basePath/AMR-Bank-v1.6_SCALA")

    val logFile = s"$basePath/diff.txt"
    val handler = new FileHandler(logFile)
    handler.setFormatter(logger.getParent.getHandlers.head.getFormatter)
    logger.addHandler(handler)

    logger.info(
      s"""Comparing folders
         |${folder1.getPath}
         |${folder2.getPath}
      """.stripMargin
    )

    val filesFolder1: Array[File] = folder1.listFiles
    val filesFolder2: Array[File] = folder2.listFiles

    // make pairs of same files in different folders
    val fileTuples = for (file1 <- filesFolder1;
                          file2 <- filesFolder2.find(_.getName == file1.getName))
      yield (file1, file2)

    val differentFiles = mutable.Set[(File, File)]()
    fileTuples.foreach {
      case (file1, file2) =>
        logger.info(s"Compairing file: ${file1.getName}")

        val lines1 = Source.fromFile(file1).getLines()
        val lines2 = Source.fromFile(file2).getLines()

        var lineId = 0
        var different = false
        lines1.zip(lines2).foreach {
          case (line1, line2) =>
            if (line1 != line2 && !ignore(line1, line2)) {
              different = true
              differentFiles.add((file1, file2))
              logger.warning(s"Line $lineId differes in both files")
              logger.warning(line1)
              logger.warning(line2)
            }
            lineId += 1
        }

        if (different) {
          logger.info("")
        }
    }

    logger.info("")
    logger.info("")
    logger.info("")
    differentFiles.foreach {
      case (file1, file2) =>
        logger.warning(s"Files differ: ${file1.getName}")
    }
  }

  def ignore(line1: String, line2: String): Boolean = {
    equalExceptDate(line1, line2) || emptyLines(line1, line2)
  }

  def equalExceptDate(line1: String, line2: String): Boolean = {
    if (!line1.startsWith("# ::alignments") || !line2.startsWith("# ::alignments"))
      return false

    val dateIdx = line1.lastIndexOf("::date")
    dateIdx >= 0 && line1.substring(0, dateIdx) == line2.substring(0, dateIdx)
  }

  def emptyLines(line1: String, line2: String): Boolean = {
    val line1Trimmed = line1.trim
    val line2Trimmed = line2.trim
    line1Trimmed == line2Trimmed && line1Trimmed == ""
  }

}
