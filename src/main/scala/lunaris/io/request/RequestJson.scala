package lunaris.io.request

import io.circe.Decoder.Result
import io.circe.generic.auto._
import io.circe.parser.decode
import io.circe.syntax._
import io.circe.{Decoder, DecodingFailure, Encoder, HCursor, Json}
import lunaris.recipes.Recipe
import lunaris.recipes.tools.native.ToolRegistry
import lunaris.recipes.tools.{Tool, ToolCall}
import lunaris.recipes.values.LunType
import lunaris.recipes.values.LunValue.PrimitiveValue
import org.broadinstitute.yootilz.core.snag.Snag

object RequestJson {

  private def newDecodingFailure(message: String, cursor: HCursor): DecodingFailure =
    DecodingFailure(message, cursor.history)

  val toolNameKey: String = "tool"

  implicit val toolCallEncoder: Encoder[ToolCall] = (toolCall: ToolCall) => {
    var fields: Map[String, Json] = Map.empty
    for ((key, arg) <- toolCall.args) {
      val jsonOpt = arg match {
        case ToolCall.ValueArg(_, PrimitiveValue.StringValue(value)) => Some(value.asJson)
        case ToolCall.ValueArg(_, PrimitiveValue.FileValue(value)) => Some(value.asJson)
        case ToolCall.ValueArg(_, PrimitiveValue.IntValue(value)) => Some(value.asJson)
        case ToolCall.ValueArg(_, PrimitiveValue.FloatValue(value)) => Some(Json.fromDoubleOrString(value))
        case ToolCall.ValueArg(_, PrimitiveValue.BoolValue(value)) => Some(value.asJson)
        case ToolCall.ValueArg(_, PrimitiveValue.UnitValue) => None
        case ToolCall.RefArg(_, ref) => Some(Json.fromString(ref))
        case ToolCall.RefArrayArg(_, refs) => Some(refs.asJson)
      }
      jsonOpt.foreach(json => fields += (key -> json))
    }
    fields += (toolNameKey -> Json.fromString(toolCall.tool.name))
    Json.fromFields(fields)
  }

  implicit val toolCallDecoder: Decoder[ToolCall] = (cursor: HCursor) => {
    cursor.downField(toolNameKey).as[String] match {
      case Left(failure) => Left(failure)
      case Right(toolName) =>
        ToolRegistry.toolsByName.get(toolName) match {
          case None => Left(newDecodingFailure(s"Do not know tool named '$toolName'.", cursor))
          case Some(tool) =>
            cursor.keys match {
              case None => Left(newDecodingFailure("No arguments", cursor))
              case Some(keys) =>
                var failureOpt: Option[DecodingFailure] = None
                var args: Map[String, ToolCall.Arg] = Map.empty
                val keysIter = keys.iterator.filter(_ != toolNameKey)
                while (failureOpt.isEmpty && keysIter.hasNext) {
                  val key = keysIter.next()
                  val argCursor = cursor.downField(key)
                  val argResult: Result[ToolCall.Arg] = tool.paramsByName.get(key) match {
                    case None =>
                      Left(newDecodingFailure(s"Tool '$toolName' does not expect an argument named '$key'.",
                        cursor))
                    case Some(valueParam: Tool.ValueParam) =>
                      val lunValueResult = valueParam.lunType match {
                        case LunType.StringType => argCursor.as[String].map(PrimitiveValue.StringValue)
                        case LunType.FileType => argCursor.as[String].map(PrimitiveValue.FileValue)
                        case LunType.IntType => argCursor.as[Long].map(PrimitiveValue.IntValue)
                        case LunType.FloatType => argCursor.as[Double].map(PrimitiveValue.FloatValue)
                        case LunType.BoolType => argCursor.as[Boolean].map(PrimitiveValue.BoolValue)
                        case LunType.UnitType => Right(PrimitiveValue.UnitValue)
                      }
                      lunValueResult.map(ToolCall.ValueArg(valueParam, _))
                    case Some(refParam: Tool.RefParam) =>
                      argCursor.as[String].map(ToolCall.RefArg(refParam, _))
                    case Some(refArrayParam: Tool.RefArrayParam) =>
                      argCursor.as[Seq[String]].map(ToolCall.RefArrayArg(refArrayParam, _))
                  }
                  argResult match {
                    case Left(decodingFailure) => failureOpt = Some(decodingFailure)
                    case Right(arg) => args += (key -> arg)
                  }
                }
                failureOpt match {
                  case Some(failure) => Left(failure)
                  case None => Right(ToolCall(tool, args))
                }
            }
        }
    }
  }

  implicit val recipeEncoder: Encoder[Recipe] = Encoder.encodeMap[String, ToolCall].contramap[Recipe](_.calls)

  implicit val recipeDecoder: Decoder[Recipe] = Decoder.decodeMap[String, ToolCall].map(Recipe)

  def parse(string: String): Either[Snag, Request] = {
    decode[Request](string).left.map(Snag(_))
  }

  def serialize(request: Request): String = request.asJson.toString
}
