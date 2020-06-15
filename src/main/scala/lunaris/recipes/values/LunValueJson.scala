package lunaris.recipes.values

import io.circe.syntax._
import io.circe.{DecodingFailure, Encoder, HCursor, Json}
import lunaris.recipes.values.LunValue.PrimitiveValue.{BoolValue, FileValue, FloatValue, IntValue, StringValue, UnitValue}
import lunaris.recipes.values.LunValue.{ArrayValue, MapValue, PrimitiveValue, RecordValue, TypeValue}
import lunaris.utils.CirceUtils

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
    case objectValue: RecordValue =>
      Json.obj(objectValue.lunType.fields.flatMap { field =>
        objectValue.values.get(field).map(_.asJson).map(json => (field, json))
      }: _*)
    case typeValue: TypeValue => typeValue.value.asString.asJson
  }

  def decodeArray(arrayCursor: HCursor, elementType: LunType): Either[DecodingFailure, LunValue.ArrayValue] = {
    (arrayCursor.value.asArray.map(_.size), arrayCursor.downArray) match {
      case (Some(size), firstElementCursor: HCursor) =>
        var index: Int = 0
        var decodingFailureOpt: Option[DecodingFailure] = None
        val builder = Seq.newBuilder[LunValue]
        var elementCursor: HCursor = firstElementCursor
        while (decodingFailureOpt.isEmpty && index < size) {
          decode(elementCursor, elementType) match {
            case Left(decodingFailure) =>
              decodingFailureOpt = Some(decodingFailure)
            case Right(elementValue) =>
              builder += elementValue
              elementCursor.right match {
                case hCursor: HCursor =>
                  elementCursor = hCursor
                  index += 1
                case cursor =>
                  decodingFailureOpt =
                    Some(CirceUtils.newDecodingFailure("Problem traversing array.", cursor))
              }
          }
        }
        decodingFailureOpt match {
          case Some(decodingFailure) => Left(decodingFailure)
          case None => Right(LunValue.ArrayValue(builder.result(), elementType))
        }
      case (_, cursor) =>
        Left(CirceUtils.newDecodingFailure("Expected array, but could not parse as array", cursor))
    }
  }

  def decodeMap(mapCursor: HCursor, valueType: LunType): Either[DecodingFailure, LunValue.MapValue] = {
    mapCursor.keys match {
      case Some(keys) =>
        var decodingFailureOpt: Option[DecodingFailure] = None
        val keyIter = keys.iterator
        val mapBuilder = Map.newBuilder[String, LunValue]
        while (decodingFailureOpt.isEmpty && keyIter.hasNext) {
          val key = keyIter.next()
          mapCursor.downField(key) match {
            case valueCursor: HCursor =>
              decode(valueCursor, valueType) match {
                case Left(decodingFailure) => decodingFailureOpt = Some(decodingFailure)
                case Right(value) => mapBuilder += (key -> value)
              }
            case failedCursor =>
              decodingFailureOpt =
                Some(CirceUtils.newDecodingFailure(s"Could not descend to property $key.", failedCursor))
          }
        }
        decodingFailureOpt match {
          case Some(decodingFailure) => Left(decodingFailure)
          case None => Right(LunValue.MapValue(mapBuilder.result(), valueType))

        }
      case None => Left(CirceUtils.newDecodingFailure(s"Expected JSON object.", mapCursor))
    }
  }

  def decode(cursor: HCursor, lunType: LunType): Either[DecodingFailure, LunValue] = {
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
      case LunType.ArrayType(elementType) => decodeArray(cursor, elementType)
      case LunType.MapType(valueType) => decodeMap(cursor, valueType)
      case _ =>
        Left(
          CirceUtils.newDecodingFailure(s"Don't know how to parse value fo type ${lunType.asString}", cursor)
        )
    }
  }

  def toJson(lunValue: LunValue): String = lunValue.asJson.toString
}
