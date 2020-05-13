package lunaris.streams.tools

import org.broadinstitute.yootilz.core.snag.Snag

object ToolsChecker {

  def checkTools(tools: Map[String, ToolCall]): Either[Snag, Unit] = {
    for {
      _ <- checkRefsExist(tools)
      _ <- checkNoCycles(tools)
      _ <- checkRequiredArgs(tools)
      _ <- checkArgTypes(tools)
    } yield ()
  }

  def checkRefsExist(tools: Map[String, ToolCall]): Either[Snag, Unit] = {
    val toolKeys = tools.keySet
    var snagOpt: Option[Snag] = None
    for ((toolKey, call) <- tools) {
      for ((argKey, arg) <- call.args) {
        arg match {
          case ToolCall.RefArg(_, ref) =>
            if (!toolKeys(ref)) {
              snagOpt = Some(Snag(s"Argument '$argKey' of tool '$toolKey' refers to non-existing tool '$ref'"))
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

  def checkNoCycles(tools: Map[String, ToolCall]): Either[Snag, Unit] = {
    var remainingTools: Set[String] = tools.keySet
    var makingProgress: Boolean = true
    while (makingProgress) {
      val remainingToolsNew = remainingTools.map(tools).flatMap(_.args.values.toSet).collect {
        case ToolCall.RefArg(_, ref) => ref
      }
      makingProgress = remainingToolsNew.size < remainingTools.size
      remainingTools = remainingToolsNew
    }
    if (remainingTools.nonEmpty) {
      Left(Snag(s"There is one or more cycles involving tools ${remainingTools.mkString("'", ", ", "'")}"))
    } else {
      Right(())
    }
  }

  def checkRequiredArgs(tools: Map[String, ToolCall]): Either[Snag, Unit] = {
    var snagOpt: Option[Snag] = None
    for ((toolKey, call) <- tools) {
      call.tool.params.find(param => param.isRequired && !call.args.keySet(param.name)).foreach { param =>
        snagOpt = Some(Snag(s"Required parameter '${param.name}' of tool '$toolKey' is missing"))
      }
    }
    snagOpt match {
      case Some(snag) => Left(snag)
      case None => Right(())
    }
  }

  def checkArgTypes(tools: Map[String, ToolCall]): Either[Snag, Unit] = {
    var snagOpt: Option[Snag] = None
    for ((toolKey, call) <- tools) {
      for ((argKey, arg) <- call.args) {
        val (argType, paramType) = arg match {
          case ToolCall.ValueArg(param, value) => (value.lunType, param.lunType)
          case ToolCall.RefArg(param, ref) => (tools(ref).tool.resultType, param.lunType)
        }
        if (!paramType.canBeAssignedFrom(argType)) {
          snagOpt =
            Some(Snag(s"Argument '$argKey' of tool '$toolKey' has type ${argType.asString}"
              + s" which is not compatible with parameter type '${paramType.asString}'.'"))
        }
      }
    }
    snagOpt match {
      case Some(snag) => Left(snag)
      case None => Right(())
    }
  }
}
