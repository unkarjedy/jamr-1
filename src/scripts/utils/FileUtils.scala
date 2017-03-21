package scripts.utils

import java.io.File
import java.nio.file.Files

import org.apache.commons.io.FilenameUtils


object FileUtils {

  def mkDir(dirPath: String): File = {
    val file = new File(dirPath)
    file.mkdirs()
    file
  }

  def saveFiles(baseFolder: String, saveToFolder: String, filesToSave: Seq[String]): Unit = {
    val modelPath = new File(baseFolder).toPath
    val stashPath = FileUtils.mkDir(s"$baseFolder/$saveToFolder").toPath

    filesToSave
      .map(modelPath.resolve)
      .map(_.toFile)
      .filter(_.exists)
      .filter(_.isFile)
      .foreach(file => {
        val name = FilenameUtils.getBaseName(file.getName)
        val ext = FilenameUtils.getExtension(file.getName)
        val copyPath = stashPath.resolve(s"${name}_${file.lastModified()}.$ext")
        if (copyPath.toFile.exists()) {
          copyPath.toFile.delete()
        }
        Files.copy(file.toPath, copyPath)
      })
  }

}
