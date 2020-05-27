package lunaris.recipes.values

import io.circe.{ACursor, DecodingFailure, Encoder, Json}
import io.circe.syntax._
import lunaris.recipes.values.LunValue.PrimitiveValue.{BoolValue, FileValue, FloatValue, IntValue, StringValue, UnitValue}
import lunaris.recipes.values.LunValue.{ArrayValue, MapValue, ObjectValue, PrimitiveValue, TypeValue}

object LunValueJson {

  implicit def valueEncoder: Encoder[LunValue] = {
    case stringValue: StringValue => stringValue.value.asJson
    case fileValue: FileValue => fileValue.value.asJson
    case intValue: IntValue => intValue.value.asJson
    case floatValue: FloatValue => floatValue.value.asJson
    case boolValue: BoolValue => boolValue.value.asJson
    case UnitValue => "()".asJson
    case arrayValue: ArrayValue => arrayValue.values.asJson
    case mapValue: MapValue => mapValue.values.asJson
    case objectValue: ObjectValue =>
      Json.obj(objectValue.lunType.fields.flatMap { field =>
        objectValue.values.get(field).map(_.asJson).map(json => (field, json))
      }: _*)
    case typeValue: TypeValue => typeValue.value.asString.asJson
  }

  def decode(cursor: ACursor, lunType: LunType): Either[DecodingFailure, LunValue] = {
    lunType match {
      case LunType.StringType => cursor.as[String].map(PrimitiveValue.StringValue)
      case LunType.FileType => cursor.as[String].map(PrimitiveValue.FileValue)
      case LunType.IntType => cursor.as[Long].map(PrimitiveValue.IntValue)
      case LunType.FloatType => cursor.as[Double].map(PrimitiveValue.FloatValue)
      case LunType.BoolType => cursor.as[Boolean].map(PrimitiveValue.BoolValue)
      case LunType.UnitType => Right(PrimitiveValue.UnitValue)
      case LunType.TypeType => cursor.as[String].flatMap { string =>
        LunType.parse(string) match {
          case Left(snag) => Left(DecodingFailure(snag.message, cursor.history))
          case Right(lunType) => Right(LunValue.TypeValue(lunType))
        }
      }
    }
  }

  def toJson(lunValue: LunValue): String = lunValue.asJson.toString
}
