package lunaris.recipes.values

import lunaris.expressions.LunExpression
import lunaris.genomics.Locus
import lunaris.io.{InputId, OutputId}
import lunaris.utils.{EitherSeqUtils, NumberParser}
import org.broadinstitute.yootilz.core.snag.Snag

sealed trait LunValue {
  def lunType: LunType

  protected def snagCannotCastTo(newType: LunType): Snag =
    Snag(s"Cannot cast this value of type ${lunType.asString} to ${newType.asString}.")

  def castTo(newType: LunType): Either[Snag, LunValue] = {
    if (newType == lunType) {
      Right(this)
    } else if (newType == LunType.UnitType) {
      Right(LunValue.PrimitiveValue.UnitValue)
    } else {
      Left(snagCannotCastTo(newType))
    }
  }

  def asString: Either[Snag, String] = Left(Snag(s"Need value of type String, but type is $lunType."))

  def asInputId: Either[Snag, InputId] = Left(Snag(s"Need value of type File, but type is $lunType."))

  def asOutputId: Either[Snag, OutputId] = Left(Snag(s"Need value of type File, but type is $lunType."))

  def asLong: Either[Snag, Long] = Left(Snag(s"Need value of type Int, but type is $lunType."))

  def asDouble: Either[Snag, Double] = Left(Snag(s"Need value of type Float, but type is $lunType."))

  def asBoolean: Either[Snag, Boolean] = Left(Snag(s"Need value of type Bool, but type is $lunType."))
}

object LunValue {

  sealed trait PrimitiveValue extends LunValue {
    def value: Any

    override def lunType: LunType.PrimitiveType

    override protected def snagCannotCastTo(newType: LunType): Snag =
      Snag(s"Cannot cast $value of type ${lunType.asString} to ${newType.asString}.")

    override def asString: Either[Snag, String] = Right(value.toString)
  }

  object PrimitiveValue {

    sealed trait LunTypedPrimitiveValue[V] extends PrimitiveValue {
      override def value: V
    }

    case class StringValue(value: String) extends LunTypedPrimitiveValue[String] {
      override val lunType: LunType.StringType.type = LunType.StringType

      override def asString: Right[Snag, String] = Right(value)

      override def castTo(newType: LunType): Either[Snag, LunValue] = {
        newType match {
          case LunType.StringType => Right(this)
          case LunType.FileType => Right(LunValue.PrimitiveValue.FileValue(value))
          case LunType.IntType => NumberParser.parseLong(value).map(LunValue.PrimitiveValue.IntValue)
          case LunType.FloatType => NumberParser.parseDouble(value).map(LunValue.PrimitiveValue.FloatValue)
          case LunType.BoolType => value match {
            case "true" => Right(BoolValue(true))
            case "false" => Right(BoolValue(false))
            case _ => Left(snagCannotCastTo(newType))
          }
          case LunType.UnitType => Right(LunValue.PrimitiveValue.UnitValue)
          case _ => Left(snagCannotCastTo(newType))
        }
      }
    }

    case class FileValue(value: String) extends LunTypedPrimitiveValue[String] {
      override val lunType: LunType.FileType.type = LunType.FileType

      override def asInputId: Right[Snag, InputId] = Right(InputId(value))

      override def asOutputId: Right[Snag, OutputId] = Right(OutputId(value))

      override def castTo(newType: LunType): Either[Snag, LunValue] = {
        newType match {
          case LunType.FileType => Right(this)
          case LunType.StringType => Right(LunValue.PrimitiveValue.StringValue(value))
          case LunType.UnitType => Right(LunValue.PrimitiveValue.UnitValue)
          case _ => Left(snagCannotCastTo(newType))
        }
      }
    }

    case class IntValue(value: Long) extends LunTypedPrimitiveValue[Long] {
      override val lunType: LunType.IntType.type = LunType.IntType

      override def asLong: Right[Snag, Long] = Right(value)

      override def asDouble: Either[Snag, Double] = Right(value.toDouble)

      override def castTo(newType: LunType): Either[Snag, LunValue] = {
        newType match {
          case LunType.IntType => Right(this)
          case LunType.FloatType => Right(LunValue.PrimitiveValue.FloatValue(value.toDouble))
          case LunType.UnitType => Right(LunValue.PrimitiveValue.UnitValue)
          case LunType.StringType => Right(LunValue.PrimitiveValue.StringValue(value.toString))
          case _ => Left(snagCannotCastTo(newType))
        }
      }
    }

    case class FloatValue(value: Double) extends LunTypedPrimitiveValue[Double] {
      override val lunType: LunType.FloatType.type = LunType.FloatType

      override def asDouble: Right[Snag, Double] = Right(value)

      override def castTo(newType: LunType): Either[Snag, LunValue] = {
        newType match {
          case LunType.FloatType => Right(this)
          case LunType.IntType => Right(LunValue.PrimitiveValue.IntValue(value.toLong))
          case LunType.UnitType => Right(LunValue.PrimitiveValue.UnitValue)
          case LunType.StringType => Right(LunValue.PrimitiveValue.StringValue(value.toString))
          case _ => Left(snagCannotCastTo(newType))
        }
      }
    }

    case class BoolValue(value: Boolean) extends LunTypedPrimitiveValue[Boolean] {
      override val lunType: LunType.BoolType.type = LunType.BoolType

      override def asBoolean: Right[Snag, Boolean] = Right(value)

      override def castTo(newType: LunType): Either[Snag, LunValue] = {
        newType match {
          case LunType.BoolType => Right(this)
          case LunType.UnitType => Right(LunValue.PrimitiveValue.UnitValue)
          case LunType.StringType => Right(LunValue.PrimitiveValue.StringValue(value.toString))
          case _ => Left(snagCannotCastTo(newType))
        }
      }
    }

    object UnitValue extends LunTypedPrimitiveValue[Unit] {
      override def value: Unit = ()

      override def lunType: LunType.UnitType.type = LunType.UnitType
    }

  }

  case class ArrayValue(values: Seq[LunValue], elementType: LunType) extends LunValue {
    override def lunType: LunType.ArrayType = LunType.ArrayType(elementType)

    override def castTo(newType: LunType): Either[Snag, LunValue] = {
      newType match {
        case LunType.ArrayType(newElementType) =>
          EitherSeqUtils.traverse(values)(_.castTo(newElementType)) match {
            case Left(snag) => Left(snag.prefix("Cannot case Array"))
            case Right(valuesCast) => Right(ArrayValue(valuesCast, newElementType))
          }
        case LunType.UnitType => Right(LunValue.PrimitiveValue.UnitValue)
        case _ => Left(snagCannotCastTo(newType))
      }
    }
  }

  case class MapValue(values: Map[String, LunValue], valueType: LunType) extends LunValue {
    override def lunType: LunType = LunType.MapType(valueType)
  }

  object MapValue {
    def emptyMap(valueType: LunType): MapValue = MapValue(Map.empty, valueType)
  }

  case class RecordValue(id: String, locus: Locus, lunType: LunType.RecordType,
                         values: Map[String, LunValue]) extends LunValue {
    override def castTo(newType: LunType): Either[Snag, LunValue] = {
      newType match {
        case newRecordType: LunType.RecordType =>
          if (newRecordType.canBeAssignedFrom(lunType)) {
            val snagOrNewValues = EitherSeqUtils.traverse(values.toSeq) { entry =>
              val key = entry._1
              val value = entry._2
              newRecordType.elementTypes.get(key) match {
                case Some(newElementType) => value.castTo(newElementType).map((key, _))
                case None => Right((key, value))
              }
            }
            snagOrNewValues.map(newValues => RecordValue(id, locus, newRecordType, newValues.toMap))
          } else {
            Left(snagCannotCastTo(newType))
          }
        case LunType.UnitType => Right(LunValue.PrimitiveValue.UnitValue)
        case _ => Left(snagCannotCastTo(newType))
      }
    }

    def has(field: String): Boolean = values.contains(field)

    def get(field: String): Either[Snag, LunValue] = {
      values.get(field) match {
        case None => Left(Snag(s"Record $id doesn't have a value for field '$field'."))
        case Some(value) => Right(value)
      }
    }

    def keepOnlyPrimitiveFields: RecordValue = {
      val fieldsNew = lunType.fields.filter(field => lunType.elementTypes.get(field).exists(_.isPrimitive))
      copy(lunType = lunType.copy(fields = fieldsNew))
    }

    def joinWith(oRecord: RecordValue): Either[Snag, RecordValue] = {
      if(id != oRecord.id) {
        Left(Snag(s"Can only join objects with same id, but ${oRecord.id} is different from $id."))
      } else if(locus != oRecord.locus) {
        Left(Snag(s"Can only join objects with same locus, but ${oRecord.locus} is different from $locus."))
      } else {
        Right(RecordValue(id, locus, lunType.joinWith(oRecord.lunType), oRecord.values ++ values))
      }
    }

    def castFieldsTo(types: Map[String, LunType]): Either[Snag, RecordValue] =
      castTo(lunType.changeFieldTypesTo(types)).map(_.asInstanceOf[RecordValue])

    def addField(fieldName: String, value: LunValue, fieldType: LunType): Either[Snag, RecordValue] = {
      if(lunType.fields.contains(fieldName)) {
        Left(Snag(s"Cannot add field $fieldName, because it already exists."))
      } else {
        Right(copy(values = values + (fieldName -> value), lunType = lunType.addField(fieldName, fieldType)))
      }
    }

    def changeIdFieldTo(idFieldNew: String): Either[Snag, RecordValue] = {
      for {
        lunTypeNew <- lunType.changeIdFieldTo(idFieldNew)
        idValueNew <-
          values.get(idFieldNew) match {
            case None => Left(Snag(s"Record has no value for $idFieldNew."))
            case Some(valueNew) => Right(valueNew)
          }
        idValueNewAsStringValue <- idValueNew.castTo(LunType.StringType)
        idNew <- idValueNewAsStringValue.asString
        valuesNew = values + (idFieldNew -> idValueNewAsStringValue)
      } yield copy(id = idNew, values = valuesNew, lunType = lunTypeNew)
    }

    def addAsNewId(idFieldNew: String, idNew: String): Either[Snag, RecordValue] = {
      for {
        recordWithNewField <- addField(idFieldNew, LunValue.PrimitiveValue.StringValue(idNew), LunType.StringType)
        recordNew <- recordWithNewField.changeIdFieldTo(idFieldNew)
      } yield recordNew
    }
  }

  case class ExpressionValue(value: LunExpression) extends LunValue {
    override def lunType: LunType = LunType.ExpressionType(value.returnType)
  }

  case class TypeValue(value: LunType) extends LunValue {
    override def lunType: LunType = LunType.TypeType
  }

}
