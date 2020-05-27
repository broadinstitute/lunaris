package lunaris.recipes.values

import io.circe.{Encoder, Json}
import io.circe.syntax._
import lunaris.recipes.values.LunValue.PrimitiveValue.{BoolValue, FileValue, FloatValue, IntValue, StringValue, UnitValue}
import lunaris.recipes.values.LunValue.{ArrayValue, MapValue, ObjectValue, TypeValue}

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

  def toJson(lunValue: LunValue): String = lunValue.asJson.toString
}
