package scripts.utils.context

import java.io.File
import java.nio.file.Files

// This class is some analogue of shell configurations from shell scripts folder
class Context {

  var jamrRoot: String = _

  // Training data inputs
  var trainFile: String = _
  var devFile: String = _
  var testFile: String = _

  // Output folder for trained weights of stage1 and stage 2
  var modelFolder: String = _
  var stage1Weights: String = _
  var stage2Weights: String = _

  var stage1Features: String = _

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
}
