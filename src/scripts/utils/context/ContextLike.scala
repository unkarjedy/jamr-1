package scripts.utils.context

import scripts.train.RunProperties

abstract class ContextLike(context: Context) {
  protected val runProperties: RunProperties = context.runProperties
}
