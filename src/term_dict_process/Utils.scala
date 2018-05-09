package term_dict_process

import scala.language.reflectiveCalls

object Utils {

  def using[A, B <: {def close()}](resource: B)(block: B => A) = {
    try {
      block(resource)
    } finally {
      resource.close()
    }
  }

}
