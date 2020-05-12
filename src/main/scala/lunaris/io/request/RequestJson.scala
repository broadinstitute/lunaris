package lunaris.io.request

import io.circe.Decoder.Result
import io.circe.{Decoder, DecodingFailure, Encoder, HCursor, Json}
import io.circe.generic.auto._
import io.circe.parser.decode
import org.broadinstitute.yootilz.core.snag.Snag
import io.circe.syntax._
import lunaris.streams.tools.ToolCall.{RefArg, ValueArg}
import lunaris.streams.tools.{Tool, ToolCall}
import lunaris.streams.tools.native.ToolRegistry
import lunaris.streams.values.{LunPrimitiveValue, LunType}

object RequestJson {

  private def newDecodingFailure(message: String, cursor: HCursor): DecodingFailure =
    DecodingFailure(message, cursor.history)

  val toolNameKey: String = "tool"

  implicit val toolCallEncoder: Encoder[ToolCall] = (toolCall: ToolCall) => {
    var fields: Map[String, Json] = Map.empty
    for ((key, arg) <- toolCall.args) {
      val jsonOpt = arg match {
        case ToolCall.ValueArg(_, LunPrimitiveValue.StringValue(value)) => Some(Json.fromString(value))
        case ToolCall.ValueArg(_, LunPrimitiveValue.FileValue(value)) => Some(Json.fromString(value))
        case ToolCall.ValueArg(_, LunPrimitiveValue.IntValue(value)) => Some(Json.fromLong(value))
        case ToolCall.ValueArg(_, LunPrimitiveValue.FloatValue(value)) => Json.fromDouble(value)
        case ToolCall.ValueArg(_, LunPrimitiveValue.BoolValue(value)) => Some(Json.fromBoolean(value))
        case ToolCall.ValueArg(_, LunPrimitiveValue.UnitValue) => None
        case ToolCall.RefArg(_, ref) => Some(Json.fromString(ref))
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
                        case LunType.StringType => argCursor.as[String].map(LunPrimitiveValue.StringValue)
                        case LunType.FileType => argCursor.as[String].map(LunPrimitiveValue.FileValue)
                        case LunType.IntType => argCursor.as[Long].map(LunPrimitiveValue.IntValue)
                        case LunType.FloatType => argCursor.as[Double].map(LunPrimitiveValue.FloatValue)
                        case LunType.BoolType => argCursor.as[Boolean].map(LunPrimitiveValue.BoolValue)
                        case LunType.UnitType => Right(LunPrimitiveValue.UnitValue)
                      }
                      lunValueResult.map(ToolCall.ValueArg(valueParam, _))
                    case Some(refParam: Tool.RefParam) =>
                      argCursor.as[String].map(ToolCall.RefArg(refParam, _))
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

  def parse(string: String): Either[Snag, Request] = {
    decode[Request](string).left.map(Snag(_))
  }

  def serialize(request: Request): String = request.asJson.toString
}
