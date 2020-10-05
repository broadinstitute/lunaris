package lunaris.recipes.values

import org.broadinstitute.yootilz.core.snag.Snag

trait LunType {
  def isPrimitive: Boolean = false

  def canBeAssignedFrom(oType: LunType): Boolean = oType == this

  def asString: String

  def lub(oType: LunType): LunType = {
    if (oType == this) {
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
      case "Stream[Record]" => Right(RecordStreamType)
      case _ =>
        if (string.startsWith("Array[") && string.endsWith("]")) {
          val elementTypeString = string.substring(6, string.length - 1)
          parse(elementTypeString).map(ArrayType(_))
        } else if (string.startsWith("Map[") && string.endsWith("]")) {
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

  object RecordStreamType extends LunType {
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
      val elementType = if (elementTypes.isEmpty) {
        AnyType
      } else {
        var lub = elementTypes.head
        for (type2 <- elementTypes.tail) {
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

  case class RecordSpecialFields(id: String, chrom: String, begin: String, end: String) {
    def changeIdFieldTo(idFieldNew: String): Either[Snag, RecordSpecialFields] = {
      if (idFieldNew.isBlank) {
        Left(Snag(s"Attempting to set record id field to a blank String, which is illegal."))
      } else if (idFieldNew == begin) {
        Left(Snag("Attempting to set record id field to begin field, which is illegal."))
      } else if (idFieldNew == end) {
        Left(Snag("Attempting to set record id field to end field, which is illegal."))
      } else {
        Right(copy(id = idFieldNew))
      }
    }
  }

  case class RecordType(specialFields: RecordSpecialFields,
                        fields: Seq[String], elementTypes: Map[String, LunType]) extends LunType {
    override def canBeAssignedFrom(oType: LunType): Boolean = {
      oType match {
        case RecordType(oSpecialFields, _, oElementTypes) =>
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

    def joinWith(oType: RecordType): RecordType = {
      val fieldsSet = fields.toSet
      val fieldsNew = fields ++ oType.fields.filter(!fieldsSet(_))
      val elementTypesNew = oType.elementTypes ++ elementTypes
      RecordType(specialFields, fieldsNew, elementTypesNew)
    }

    def addField(field: String, lunType: LunType): RecordType =
      copy(fields = fields :+ field, elementTypes = elementTypes + (field -> lunType))

    def changeFieldTypesTo(types: Map[String, LunType]): RecordType = {
      val eternalTypes =
        Map(
          specialFields.id -> LunType.StringType,
          specialFields.chrom -> LunType.StringType,
          specialFields.begin -> LunType.IntType,
          specialFields.end -> LunType.IntType
        )
      copy(elementTypes = elementTypes ++ types ++ eternalTypes)
    }

    def changeIdFieldTo(idFieldNameNew: String): Either[Snag, RecordType] = {
      for {
        specialFieldsNew <- specialFields.changeIdFieldTo(idFieldNameNew)
        elementTypesNew <-
          elementTypes.get(idFieldNameNew).filterNot(StringType.canBeAssignedFrom) match {
            case Some(lunType) => Left(Snag(s"Id fields needs to be assignable to String, but is $lunType."))
            case None => Right(elementTypes + (idFieldNameNew -> StringType))
          }
      } yield copy(specialFields = specialFieldsNew, elementTypes = elementTypesNew)
    }
  }

  object RecordType {
    def apply(id: String, chrom: String, begin: String, end: String): RecordType = {
      RecordType(RecordSpecialFields(id, chrom, begin, end), Seq(id, chrom, begin, end),
        Map(id -> StringType, chrom -> StringType, begin -> IntType, end -> IntType))
    }
  }

  case class ExpressionType(returnType: LunType) extends LunType {
    override def asString: String = s"Expression[${returnType.asString}]"
  }

  object AnyType extends LunType {
    override def canBeAssignedFrom(oType: LunType): Boolean = true

    override def asString: String = "Any"
  }

  object TypeType extends LunType {
    override def asString: String = "Type"
  }

}
