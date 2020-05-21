package lunaris.recipes.values

trait LunType {
  def isPrimitive: Boolean = false
  def canBeAssignedFrom(oType: LunType): Boolean = oType == this

  def asString: String
}

object LunType {

  sealed trait PrimitiveType extends LunType {
    override def isPrimitive: Boolean = true
  }

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

  object ObjectStreamType extends LunType {
    override def asString: String = "Stream[Object]"
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

  case class ObjectSpecialFields(id: String, chrom: String, begin: String, end: String)

  case class ObjectType(specialFields: ObjectSpecialFields,
                        fields: Seq[String], elementTypes: Map[String, LunType]) extends LunType {
    override def canBeAssignedFrom(oType: LunType): Boolean = {
      oType match {
        case ObjectType(oSpecialFields, _, oElementTypes) =>
          (specialFields == oSpecialFields) && elementTypes.collect {
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

    def joinWith(oType: ObjectType): ObjectType = {
      val fieldsSet = fields.toSet
      val fieldsNew = fields ++ oType.fields.filter(!fieldsSet(_))
      val elementTypesNew = oType.elementTypes ++ elementTypes
      ObjectType(specialFields, fieldsNew, elementTypesNew)
    }
  }

  object ObjectType {
    def apply(id: String, chrom: String, begin: String, end: String): ObjectType = {
      ObjectType(ObjectSpecialFields(id, chrom, begin, end), Seq(id, chrom, begin, end),
        Map(id -> StringType, chrom -> StringType, begin -> IntType, end -> IntType))
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
