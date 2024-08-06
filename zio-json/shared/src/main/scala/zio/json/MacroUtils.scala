package zio.json

object MacroUtils {
  //Each default option is an Option[Either[A,B]] where A is the standard, precomputed default and B is a function to calculate the default each time
  //This is necessary to support both defaults that can be precomputed as well as others (e.g. random generators such as UUID.randomUUID()) that need to be freshly calculated each time
  private[json] type DefaultOption = Option[Either[Any, () => Any]]

  private[json] def calculateDefaultOption(
    defaultEvaluatorOption: Option[() => Any],
    defaultOption: Option[Any],
    isAlwaysEvaluateAnnotationPresent: Boolean
  ): DefaultOption =
    defaultEvaluatorOption.flatMap { defaultEvaluator =>
      val sampleEvaluation = defaultEvaluator()
      if (isAlwaysEvaluateAnnotationPresent || sampleEvaluation != defaultEvaluator()) {
        Some(Right(defaultEvaluator))
      } else {
        sampleEvaluation match {
          case _: java.time.temporal.Temporal => Some(Right(defaultEvaluator))
          case _                              => None
        }
      }
    }.orElse(defaultOption.map(Left(_)))
}
