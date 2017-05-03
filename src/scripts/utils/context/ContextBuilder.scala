package scripts.utils.context

import edu.cmu.lti.nlp.amr.Wordnet

//TODO: replace string concat with Path.resolve
object ContextBuilder {

  def createContext(jamrRoot: String, modelFolder: String): Context = {
    val toolsPath = s"$jamrRoot/tools/"

    val context = new Context()
    context.jamrRoot = jamrRoot
    context.modelFolder = modelFolder

    context.cdecPath = s"$toolsPath/cdec"
    context.illinoisNerPath = s"$toolsPath/IllinoisNerExtended/"
    context.illinoisNerJarPath = s"${context.illinoisNerPath}/target/IllinoisNerExtended-2.7.jar"
    context.nerConfigPath = s"$jamrRoot/scripts/preprocessing/IllinoisNER.config"
    context.smatchPath = s"$jamrRoot/scripts/smatch_v1_0/smatch_modified.py"
    context.wnetPath = s"$toolsPath/WordNet-3.0/"

    context.normalizePaths()

    Wordnet.prepareWordNetStemmer(context.wnetPath)

    context
  }

  def createContextForTraining(jamrRoot: String,
                               baseDataDir: String,
                               baseDataFileName: String,
                               modelFolder: String): Context = {
    // role = dev/test/train
    def getFilePathWithRole(role: String) = s"$baseDataDir$baseDataFileName-$role.txt"

    val context = createContext(jamrRoot, modelFolder)
    context.trainFile = getFilePathWithRole("training")
    context.devFile = getFilePathWithRole("dev")
    context.testFile = getFilePathWithRole("test")
    context
  }
}
