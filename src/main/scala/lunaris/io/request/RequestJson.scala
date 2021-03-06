package lunaris.io.request

import io.circe.Decoder.Result
import io.circe.generic.auto._
import io.circe.parser.decode
import io.circe.syntax._
import io.circe.{Decoder, DecodingFailure, Encoder, HCursor, Json}
import lunaris.recipes.Recipe
import lunaris.recipes.tools.builtin.ToolRegistry
import lunaris.recipes.tools.{Tool, ToolCall}
import lunaris.recipes.values.LunValueJson
import lunaris.recipes.values.LunValueJson.valueEncoder
import lunaris.utils.CirceUtils
import org.broadinstitute.yootilz.core.snag.Snag

object RequestJson {

  val toolNameKey: String = "tool"

  implicit val toolCallEncoder: Encoder[ToolCall] = (toolCall: ToolCall) => {
    var fields: Map[String, Json] = Map.empty
    for ((key, arg) <- toolCall.args) {
      val json = arg match {
        case ToolCall.ValueArg(_, value) => value.asJson
        case ToolCall.RefArg(_, ref) => ref.asJson
        case ToolCall.RefArrayArg(_, refs) => refs.asJson
      }
      fields += (key -> json)
    }
    fields += (toolNameKey -> Json.fromString(toolCall.tool.name))
    Json.fromFields(fields)
  }

  implicit val toolCallDecoder: Decoder[ToolCall] = (cursor: HCursor) => {
    cursor.downField(toolNameKey).as[String] match {
      case Left(failure) => Left(failure)
      case Right(toolName) =>
        ToolRegistry.toolsByName.get(toolName) match {
          case None => Left(CirceUtils.newDecodingFailure(s"Do not know tool named '$toolName'.", cursor))
          case Some(tool) =>
            cursor.keys match {
              case None => Left(CirceUtils.newDecodingFailure("No arguments", cursor))
              case Some(keys) =>
                var failureOpt: Option[DecodingFailure] = None
                var args: Map[String, ToolCall.Arg] = Map.empty
                val keysIter = keys.iterator.filter(_ != toolNameKey)
                while (failureOpt.isEmpty && keysIter.hasNext) {
                  val key = keysIter.next()
                  cursor.downField(key) match {
                    case argCursor: HCursor =>
                      val argResult: Result[ToolCall.Arg] = tool.paramsByName.get(key) match {
                        case None =>
                          Left(
                            CirceUtils.newDecodingFailure(
                              s"Tool '$toolName' does not expect an argument named '$key'.", cursor)
                          )
                        case Some(valueParam: Tool.ValueParam) =>
                          LunValueJson.decode(argCursor, valueParam.lunType).map(ToolCall.ValueArg(valueParam, _))
                        case Some(refParam: Tool.RefParam) =>
                          argCursor.as[String].map(ToolCall.RefArg(refParam, _))
                        case Some(refArrayParam: Tool.RefArrayParam) =>
                          argCursor.as[Seq[String]].map(ToolCall.RefArrayArg(refArrayParam, _))
                      }
                      argResult match {
                        case Left(decodingFailure) => failureOpt = Some(decodingFailure)
                        case Right(arg) => args += (key -> arg)
                      }
                    case cursor =>
                      Left(CirceUtils.newDecodingFailure(s"Failure trying to parse argument $key.", cursor))
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
