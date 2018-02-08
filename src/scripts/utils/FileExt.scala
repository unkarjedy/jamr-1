package scripts.utils

import java.io.File

object FileExt {
  implicit class RichFile(val file: File) extends AnyVal {
    def resolve(name: String): File = file.toPath.resolve(name).toFile
  }
}
