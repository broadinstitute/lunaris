package lunaris.recipes.values

sealed trait LunPrimitiveValue {
  def value: Any

  def lunType: LunType.PrimitiveType
}

object LunPrimitiveValue {

  sealed trait LunTypedPrimitiveValue[V] extends LunPrimitiveValue {
    override def value: V
  }

  case class StringValue(value: String) extends LunTypedPrimitiveValue[String] {
    override val lunType: LunType.StringType.type = LunType.StringType
  }

  case class FileValue(value: String) extends LunTypedPrimitiveValue[String] {
    override val lunType: LunType.FileType.type = LunType.FileType
  }

  case class IntValue(value: Long) extends LunTypedPrimitiveValue[Long] {
    override val lunType: LunType.IntType.type = LunType.IntType
  }

  case class FloatValue(value: Double) extends LunTypedPrimitiveValue[Double] {
    override val lunType: LunType.FloatType.type = LunType.FloatType
  }

  case class BoolValue(value: Boolean) extends LunTypedPrimitiveValue[Boolean] {
    override val lunType: LunType.BoolType.type = LunType.BoolType
  }

  object UnitValue extends LunTypedPrimitiveValue[Unit] {
    override def value: Unit = ()

    override def lunType: LunType.UnitType.type = LunType.UnitType
  }

}