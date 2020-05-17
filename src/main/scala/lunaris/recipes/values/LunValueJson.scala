package lunaris.recipes.values

import io.circe.Encoder
import io.circe.syntax._
import lunaris.recipes.values.LunValue.PrimitiveValue.{BoolValue, FileValue, FloatValue, IntValue, StringValue, UnitValue}
import lunaris.recipes.values.LunValue.{ArrayValue, ObjectValue, TypeValue}

object LunValueJson {

  implicit def valueEncoder: Encoder[LunValue] = {
    case stringValue: StringValue => stringValue.value.asJson
    case fileValue: FileValue => fileValue.value.asJson
    case intValue: IntValue => intValue.value.asJson
    case floatValue: FloatValue => floatValue.value.asJson
    case boolValue: BoolValue => boolValue.value.asJson
    case UnitValue => "()".asJson
    case arrayValue: ArrayValue => arrayValue.values.asJson
    case objectValue: ObjectValue => objectValue.values.asJson
    case typeValue: TypeValue => typeValue.value.asString.asJson
  }

  def toJson(lunValue: LunValue): String = lunValue.asJson.toString
}