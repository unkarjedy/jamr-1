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

  def saveFiles(baseFolder: String, saveToFolder: String, filesNamesToSave: Seq[String]): Unit = {
    val modelPath = new File(baseFolder).toPath
    val filesToSave = filesNamesToSave
      .map(modelPath.resolve)
      .map(_.toFile)
      .filter(_.exists)
      .filter(_.isFile)

    val saveToPath = if (filesToSave.nonEmpty) {
      FileUtils.mkDir(s"$baseFolder/$saveToFolder").toPath
    } else {
      null
    }

    filesToSave.foreach(file => {
      val name = FilenameUtils.getBaseName(file.getName)
      val extension = FilenameUtils.getExtension(file.getName)
      val copyPath = saveToPath.resolve(s"${name}_${file.lastModified()}.$extension")
      if (copyPath.toFile.exists()) {
        copyPath.toFile.delete()
      }
      Files.copy(file.toPath, copyPath)
    })
  }

}
