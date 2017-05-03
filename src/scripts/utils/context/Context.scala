package scripts.utils.context

import scripts.train.RunProperties

// This class is some analogue of shell configurations from shell scripts folder
class Context {
  var runProperties: RunProperties = _

  var jamrRoot: String = _

  // Training data inputs
  var trainFile: String = _
  var devFile: String = _
  var testFile: String = _

  // Output folder for trained weights of stage1 and stage 2
  var modelFolder: String = _
  var stage1Weights: String = _
  var stage2Weights: String = _

  var stage1Features: List[String] = _
  var stage2Features: List[String] = _

  // JAMR Parser common options
  var parserOptions: String = _
  var conceptIdTrainingOptions: String = _
  var relationIdTrainingOptions: String = _

  // TOOLS
  var cdecPath: String = _
  var wnetPath: String = _
  var illinoisNerPath: String = _
  var illinoisNerJarPath: String = _
  var nerConfigPath: String = _
  var smatchPath: String = _

  def normalizePaths(): Unit = {
    cdecPath = normalizePath(cdecPath)
    wnetPath = normalizePath(wnetPath)
    illinoisNerPath = normalizePath(illinoisNerPath)
    illinoisNerJarPath = normalizePath(illinoisNerJarPath)
    nerConfigPath = normalizePath(nerConfigPath)
    modelFolder = normalizePath(modelFolder)
    smatchPath = normalizePath(smatchPath)
  }

  private def normalizePath(path: String): String = {
    path.replace("//", "/")
      .replace("\\", "/")
  }

  def toLogString: String = {
    s"""$jamrRoot
       |$trainFile
       |$devFile
       |$testFile
       |$modelFolder
       |$stage1Weights
       |$stage2Weights
       |$stage1Features
       |$stage2Features
       |
       |$parserOptions
       |
       |$conceptIdTrainingOptions
       |
       |$relationIdTrainingOptions
     """.stripMargin
  }
}
