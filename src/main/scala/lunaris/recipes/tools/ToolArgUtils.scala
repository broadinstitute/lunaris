package lunaris.recipes.tools

import lunaris.io.{InputId, OutputId}
import lunaris.recipes.values.LunValue.PrimitiveValue
import org.broadinstitute.yootilz.core.snag.Snag

object ToolArgUtils {

  def as[T](argName: String,
            args: Map[String, ToolCall.Arg])(extract: PrimitiveValue => Either[Snag, T]): Either[Snag, T] = {
    for {
      arg <- args.get(argName).map(Right(_)).getOrElse(Left(Snag(s"Missing argument '$argName'.'")))
      string <- as[T](arg)(extract)
    } yield string
  }

  def asOpt[T](argName: String, args: Map[String, ToolCall.Arg])(extract: PrimitiveValue => Either[Snag, T]):
  Either[Snag, Option[T]] = {
    args.get(argName) match {
      case Some(arg) => as[T](arg)(extract).map(Some(_))
      case None => Right(None)
    }
  }

  def asOr[T](argName: String,
              args: Map[String, ToolCall.Arg], default: T)(
               extract: PrimitiveValue => Either[Snag, T]): Either[Snag, T] = {
    args.get(argName) match {
      case Some(arg) => as[T](arg)(extract)
      case None => Right(default)
    }
  }

  def as[T](arg: ToolCall.Arg)(
    extract: PrimitiveValue => Either[Snag, T]): Either[Snag, T] = {
    arg match {
      case ToolCall.RefArg(param, _) =>
        Left(Snag(s"Argument '${param.name}' is a reference, but should be a value."))
      case ToolCall.RefArrayArg(param, _) =>
        Left(Snag(s"Argument '${param.name}' is a reference array, but should be a value."))
      case ToolCall.ValueArg(_, value) => extract(value)
    }
  }

  def asString(name: String, args: Map[String, ToolCall.Arg]): Either[Snag, String] = as(name, args)(_.asString)

  def asStringOpt(name: String, args: Map[String, ToolCall.Arg]): Either[Snag, Option[String]] =
    asOpt(name, args)(_.asString)

  def asStringOr(name: String, args: Map[String, ToolCall.Arg], default: String): Either[Snag, String] =
    asOr(name, args, default)(_.asString)

  def asString(arg: ToolCall.Arg): Either[Snag, String] = as(arg)(_.asString)

  def asInputId(name: String, args: Map[String, ToolCall.Arg]): Either[Snag, InputId] = as(name, args)(_.asInputId)

  def asInputIdOpt(name: String, args: Map[String, ToolCall.Arg]): Either[Snag, Option[InputId]] =
    asOpt(name, args)(_.asInputId)

  def asInputIdOr(name: String, args: Map[String, ToolCall.Arg], default: InputId): Either[Snag, InputId] =
    asOr(name, args, default)(_.asInputId)

  def asInputId(arg: ToolCall.Arg): Either[Snag, InputId] = as(arg)(_.asInputId)

  def asOutputId(name: String, args: Map[String, ToolCall.Arg]): Either[Snag, OutputId] = as(name, args)(_.asOutputId)

  def asOutputIdOpt(name: String, args: Map[String, ToolCall.Arg]): Either[Snag, Option[OutputId]] =
    asOpt(name, args)(_.asOutputId)

  def asOutputIdOr(name: String, args: Map[String, ToolCall.Arg], default: OutputId): Either[Snag, OutputId] =
    asOr(name, args, default)(_.asOutputId)

  def asOutputId(arg: ToolCall.Arg): Either[Snag, OutputId] = as(arg)(_.asOutputId)

  def asRef(argName: String, args: Map[String, ToolCall.Arg]): Either[Snag, String] = {
    for {
      arg <- args.get(argName).map(Right(_)).getOrElse(Left(Snag(s"Missing argument '$argName'.'")))
      ref <- asRef(arg)
    } yield ref
  }

  def asRefs(argName: String, args: Map[String, ToolCall.Arg]): Either[Snag, Seq[String]] = {
    for {
      arg <- args.get(argName).map(Right(_)).getOrElse(Left(Snag(s"Missing argument '$argName'.'")))
      refs <- asRefs(arg)
    } yield refs
  }

  def asRef(arg: ToolCall.Arg): Either[Snag, String] = {
    arg match {
      case ToolCall.RefArg(_, ref) => Right(ref)
      case ToolCall.RefArrayArg(param, _) =>
        Left(Snag(s"Argument '${param.name}' should be a reference, but is a reference array."))
      case ToolCall.ValueArg(param, value) =>
        Left(Snag(s"Argument '${param.name}' should be a reference, but is a value ($value)."))
    }
  }

  def asRefs(arg: ToolCall.Arg): Either[Snag, Seq[String]] = {
    arg match {
      case ToolCall.RefArrayArg(param, refs) => Right(refs)
      case ToolCall.RefArg(param, _) =>
        Left(Snag(s"Argument '${param.name}' should be a reference array, but is a reference."))
      case ToolCall.ValueArg(param, value) =>
        Left(Snag(s"Argument '${param.name}' should be a reference, but is a value ($value)."))
    }
  }

}
