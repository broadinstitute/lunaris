package lunaris.recipes.values

import org.broadinstitute.yootilz.core.snag.Snag

trait LunType {
  def isPrimitive: Boolean = false
  def canBeAssignedFrom(oType: LunType): Boolean = oType == this

  def asString: String

  def lub(oType: LunType): LunType = {
    if(oType == this) {
      this
    } else {
      LunType.AnyType
    }
  }
}

object LunType {

  def parse(string: String): Either[Snag, LunType] = {
    string match {
      case "String" => Right(StringType)
      case "File" => Right(FileType)
      case "Int" => Right(IntType)
      case "Float" => Right(FloatType)
      case "Bool" => Right(BoolType)
      case "Unit" => Right(UnitType)
      case "Type" => Right(TypeType)
      case "Stream[Object]" => Right(ObjectStreamType)
      case _ =>
        if(string.startsWith("Array[") && string.endsWith("]")) {
          val elementTypeString = string.substring(6, string.length - 1)
          parse(elementTypeString).map(ArrayType(_))
        } else if(string.startsWith("Map[") && string.endsWith("]")) {
          val elementTypeString = string.substring(4, string.length - 1)
          parse(elementTypeString).map(MapType)
        } else {
          Left(Snag(s"Don't know how to parse $string into a type."))
        }
    }
  }

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

    override def canBeAssignedFrom(oType: LunType): Boolean = oType == IntType || oType == StringType
  }

  object FloatType extends NumberType {
    override val asString: String = "Float"

    override def canBeAssignedFrom(oType: LunType): Boolean = oType == FloatType || oType == StringType
  }

  object BoolType extends PrimitiveType {
    override val asString: String = "Bool"
  }

  object UnitType extends PrimitiveType {
    override def canBeAssignedFrom(oType: LunType): Boolean = true

    override val asString: String = "Unit"
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

  object ArrayType {
    def fromElementTypes(elementTypes: Seq[LunType]): ArrayType = {
      val elementType = if(elementTypes.isEmpty) {
        AnyType
      } else {
        var lub = elementTypes.head
        for(type2 <- elementTypes.tail) {
          lub = lub.lub(type2)
        }
        lub
      }
      ArrayType(elementType)
    }
  }

  case class MapType(valueType: LunType) extends LunType {
    override def asString: String = s"Map[${valueType.asString}]"
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

    def changeFieldTypesTo(types: Map[String, LunType]): ObjectType = {
      val eternalTypes =
        Map(
          specialFields.id -> LunType.StringType,
          specialFields.chrom -> LunType.StringType,
          specialFields.begin -> LunType.IntType,
          specialFields.end -> LunType.IntType
        )
      copy(elementTypes = elementTypes ++ types ++ eternalTypes)
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
