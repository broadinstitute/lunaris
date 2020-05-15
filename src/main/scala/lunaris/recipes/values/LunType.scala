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

  case class ArrayType(elementType: LunType) extends LunType {
    override def canBeAssignedFrom(oType: LunType): Boolean = {
      oType match {
        case ArrayType(oElementType) => elementType.canBeAssignedFrom(oElementType)
        case _ => false
      }
    }

    override def asString: String = s"Array[${elementType.asString}]"
  }

  case class ObjectType(elementTypes: Map[String, LunType]) extends LunType {
    override def canBeAssignedFrom(oType: LunType): Boolean = {
      oType match {
        case ObjectType(oElementTypes) =>
          elementTypes.collect {
            case (key, thisType) =>
              oElementTypes.get(key) match {
                case Some(oType) => thisType.canBeAssignedFrom(oType)
                case None => false
              }
          }.forall(identity)
        case _ => false
      }
    }

    override def asString: String = {
      val typesString = elementTypes.collect { case (key, value) => s"$key -> ${value.asString}" }.mkString(", ")
      s"Object[$typesString]"
    }
  }

  object AnyType extends LunType {
    override def canBeAssignedFrom(oType: LunType): Boolean = true

    override def asString: String = "Any"
  }

  object TypeType extends LunType {
    override def asString: String = "Type"
  }
}
