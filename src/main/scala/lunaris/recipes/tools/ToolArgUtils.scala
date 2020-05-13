package lunaris.recipes.tools

import lunaris.io.{InputId, OutputId}
import lunaris.recipes.values.{LunPrimitiveValue, LunType}
import org.broadinstitute.yootilz.core.snag.Snag

object ToolArgUtils {

  def as[T](argName: String,
            args: Map[String, ToolCall.Arg])(extract: LunPrimitiveValue => Either[Snag, T]): Either[Snag, T] = {
    for {
      arg <- args.get(argName).map(Right(_)).getOrElse(Left(Snag(s"Missing argument '$argName'.'")))
      string <- as[T](arg)(extract)
    } yield string
  }

  def asOr[T](argName: String,
            args: Map[String, ToolCall.Arg], default: T)(
    extract: LunPrimitiveValue => Either[Snag, T]): Either[Snag, T] = {
    args.get(argName) match {
      case Some(arg) => as[T](arg)(extract)
      case None => Right(default)
    }
  }

  def as[T](arg: ToolCall.Arg)(
    extract: LunPrimitiveValue => Either[Snag, T]): Either[Snag, T] = {
    arg match {
      case ToolCall.RefArg(param, _) =>
        Left(Snag(s"Argument '${param.name}' is a reference, but should be a value."))
      case ToolCall.ValueArg(_, value) => extract(value)
    }
  }

  def asString(name: String, args: Map[String, ToolCall.Arg]): Either[Snag, String] = as(name, args)(_.asString)

  def asStringOr(name: String, args: Map[String, ToolCall.Arg], default: String): Either[Snag, String] =
    asOr(name, args, default)(_.asString)

  def asString(arg: ToolCall.Arg): Either[Snag, String] = as(arg)(_.asString)

  def asInputId(name: String, args: Map[String, ToolCall.Arg]): Either[Snag, InputId] = as(name, args)(_.asInputId)

  def asInputIdOr(name: String, args: Map[String, ToolCall.Arg], default: InputId): Either[Snag, InputId] =
    asOr(name, args, default)(_.asInputId)

  def asInputId(arg: ToolCall.Arg): Either[Snag, InputId] = as(arg)(_.asInputId)

}
