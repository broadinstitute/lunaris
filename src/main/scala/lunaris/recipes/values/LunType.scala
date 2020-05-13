package lunaris.recipes.values

trait LunType {
  def canBeAssignedFrom(oType: LunType): Boolean = oType == this
  def asString: String
}

object LunType {
  sealed trait PrimitiveType extends LunType

  sealed trait StringLikeType extends PrimitiveType

  object StringType extends StringLikeType {
    override val asString: String = "String"
  }

  object FileType extends StringLikeType {
    override val asString: String = "File"
  }

  sealed trait NumberType extends PrimitiveType

  object IntType extends NumberType {
    override val asString: String = "Int"
  }

  object FloatType extends NumberType {
    override val asString: String = "Float"
  }

  object BoolType extends PrimitiveType {
    override val asString: String = "Bool"
  }

  object UnitType extends PrimitiveType {
    override def canBeAssignedFrom(oType: LunType): Boolean = true
    override val asString: String = "Unit"
  }

  object RecordStreamType extends LunType {
    override def asString: String = "Stream[Record]"
  }
}
