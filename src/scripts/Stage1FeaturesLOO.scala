package scripts

import java.io.File

import scala.io.Source

/**
  * Do not see investigate this class. It is just my playground...
   */
object Stage1FeaturesLOO {

  def main(args: Array[String]): Unit = {
    val parentFolder = new File("C:/Users/Dmitrii_Naumenko/Desktop/JAMR_2016_Github/models/Little_Prince_v1_6_LOO")
    val looFolders = parentFolder
      .listFiles()
      .filter(_.isDirectory)

    var looStats = looFolders
      .map(folder => {
        val resultsFile = folder.toPath.resolve("RESULTS.txt").toFile
        val text = Source.fromFile(resultsFile).getLines().mkString("\n")
        val leftFeature = folder.getName

        val regexp = (
          "(?s).*Stage1 evaluation.*" +
            "Precision: (.*)" +
            "Recall: (.*)" +
            "F1: (.*)"
          ).r

        val stats = text match {
          case regexp(precision, recall, f1) =>
            Stats(leftFeature, precision.trim.toDouble, recall.trim.toDouble, f1.trim.toDouble)
        }

        stats
      })

    looStats = looStats.sortBy(_.f1)
    looStats
  }

  case class Stats(leftFeture: String, precision: Double, recall: Double, f1: Double)

}
