package scripts.utils.context

abstract class ContextLike(context: Context) {
  protected val runProperties = context.runProperties
}
