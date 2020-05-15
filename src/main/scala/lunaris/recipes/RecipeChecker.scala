package lunaris.recipes

import lunaris.recipes.tools.ToolCall
import org.broadinstitute.yootilz.core.snag.Snag

object RecipeChecker {

  def checkRecipe(recipe: Recipe): Either[Snag, Unit] = {
    for {
      _ <- checkRefsExist(recipe)
      _ <- checkNoCycles(recipe)
      _ <- checkRequiredArgs(recipe)
      _ <- checkArgTypes(recipe)
      _ <- checkFinalsHaveEffects(recipe)
    } yield ()
  }

  def checkRefsExist(recipe: Recipe): Either[Snag, Unit] = {
    val callKeys = recipe.calls.keySet
    var snagOpt: Option[Snag] = None
    for ((callKey, call) <- recipe.calls) {
      for ((argKey, arg) <- call.args) {
        arg match {
          case ToolCall.RefArg(_, ref) =>
            if (!callKeys(ref)) {
              snagOpt = Some(Snag(s"Argument '$argKey' of call '$callKey' refers to non-existing call '$ref'"))
            }
          case _ => ()
        }
      }
    }
    snagOpt match {
      case Some(snag) => Left(snag)
      case None => Right(())
    }
  }

  def checkNoCycles(recipe: Recipe): Either[Snag, Unit] = {
    var remainingCalls: Set[String] = recipe.calls.keySet
    var makingProgress: Boolean = true
    while (makingProgress) {
      val remainingCallsNew = remainingCalls.map(recipe.calls).flatMap(_.args.values.toSet).collect {
        case ToolCall.RefArg(_, ref) => ref
      }
      makingProgress = remainingCallsNew.size < remainingCalls.size
      remainingCalls = remainingCallsNew
    }
    if (remainingCalls.nonEmpty) {
      Left(Snag(s"There is one or more cycles involving calls ${remainingCalls.mkString("'", ", ", "'")}"))
    } else {
      Right(())
    }
  }

  def checkRequiredArgs(recipe: Recipe): Either[Snag, Unit] = {
    var snagOpt: Option[Snag] = None
    for ((callKey, call) <- recipe.calls) {
      call.tool.params.find(param => param.isRequired && !call.args.keySet(param.name)).foreach { param =>
        snagOpt = Some(Snag(s"Required parameter '${param.name}' of tool '$callKey' is missing"))
      }
    }
    snagOpt match {
      case Some(snag) => Left(snag)
      case None => Right(())
    }
  }

  def checkArgTypes(recipe: Recipe): Either[Snag, Unit] = {
    var snagOpt: Option[Snag] = None
    for ((callKey, call) <- recipe.calls) {
      for ((argKey, arg) <- call.args) {
        val (argType, paramType) = arg match {
          case ToolCall.ValueArg(param, value) => (value.lunType, param.lunType)
          case ToolCall.RefArg(param, ref) => (recipe.calls(ref).tool.resultType, param.lunType)
        }
        if (!paramType.canBeAssignedFrom(argType)) {
          snagOpt =
            Some(Snag(s"Argument '$argKey' of call '$callKey' has type ${argType.asString}"
              + s" which is not compatible with parameter type '${paramType.asString}'.'"))
        }
      }
    }
    snagOpt match {
      case Some(snag) => Left(snag)
      case None => Right(())
    }
  }

  def checkFinalsHaveEffects(recipe: Recipe): Either[Snag, Unit] = {
    val refs = recipe.calls.values.toSet.flatMap { call: ToolCall =>
      call.args.values.collect {
        case ToolCall.RefArg(_, ref) => ref
      }
    }
    val unreferencedKeys = recipe.calls.keySet -- refs
    val unreferencedKeysWithoutEffect = unreferencedKeys.filter(key => !recipe.calls(key).tool.isFinal)
    unreferencedKeysWithoutEffect.headOption match {
      case Some(key) => Left(Snag(s"Call $key is neither referenced nor has an effect."))
      case None => Right(())
    }
  }
}
