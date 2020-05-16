package lunaris.recipes.values

import lunaris.io.{InputId, OutputId}
import org.broadinstitute.yootilz.core.snag.Snag

sealed trait LunValue {
  def lunType: LunType
}

object LunValue {

  sealed trait PrimitiveValue extends LunValue {
    def value: Any

    override def lunType: LunType.PrimitiveType

    def asString: Either[Snag, String] = Left(Snag(s"Need value of type String, but type is $lunType."))

    def asInputId: Either[Snag, InputId] = Left(Snag(s"Need value of type File, but type is $lunType."))

    def asOutputId: Either[Snag, OutputId] = Left(Snag(s"Need value of type File, but type is $lunType."))

    def asLong: Either[Snag, Long] = Left(Snag(s"Need value of type Int, but type is $lunType."))

    def asDouble: Either[Snag, Double] = Left(Snag(s"Need value of type Float, but type is $lunType."))

    def asBoolean: Either[Snag, Boolean] = Left(Snag(s"Need value of type Bool, but type is $lunType."))
  }

  object PrimitiveValue {

    sealed trait LunTypedPrimitiveValue[V] extends PrimitiveValue {
      override def value: V
    }

    case class StringValue(value: String) extends LunTypedPrimitiveValue[String] {
      override val lunType: LunType.StringType.type = LunType.StringType

      override def asString: Right[Snag, String] = Right(value)
    }

    case class FileValue(value: String) extends LunTypedPrimitiveValue[String] {
      override val lunType: LunType.FileType.type = LunType.FileType

      override def asInputId: Right[Snag, InputId] = Right(InputId(value))

      override def asOutputId: Right[Snag, OutputId] = Right(OutputId(value))
    }

    case class IntValue(value: Long) extends LunTypedPrimitiveValue[Long] {
      override val lunType: LunType.IntType.type = LunType.IntType

      override def asLong: Right[Snag, Long] = Right(value)
    }

    case class FloatValue(value: Double) extends LunTypedPrimitiveValue[Double] {
      override val lunType: LunType.FloatType.type = LunType.FloatType

      override def asDouble: Right[Snag, Double] = Right(value)
    }

    case class BoolValue(value: Boolean) extends LunTypedPrimitiveValue[Boolean] {
      override val lunType: LunType.BoolType.type = LunType.BoolType

      override def asBoolean: Right[Snag, Boolean] = Right(value)
    }

    object UnitValue extends LunTypedPrimitiveValue[Unit] {
      override def value: Unit = ()

      override def lunType: LunType.UnitType.type = LunType.UnitType
    }

  }

  case class ArrayValue(values: Seq[LunValue], elementType: LunType) extends LunValue {
    override def lunType: LunType.ArrayType = LunType.ArrayType(elementType)
  }

  case class ObjectValue(lunType: LunType.ObjectType, values: Map[String, LunValue]) extends LunValue

  case class TypeValue(value: LunType) extends LunValue {
    override def lunType: LunType = LunType.TypeType
  }
}
