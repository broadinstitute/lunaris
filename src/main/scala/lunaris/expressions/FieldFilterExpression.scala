package lunaris.expressions

import lunaris.recipes.values.{LunType, LunValue}
import lunaris.recipes.values.LunValue.PrimitiveValue
import org.broadinstitute.yootilz.core.snag.Snag
import scala.util.matching.Regex


trait FieldFilterExpression extends LunBoolExpression {
  def field: String

  override def fields: Set[String] = Set(field)

  override def cleaned: FieldFilterExpression
}

object FieldFilterExpression {

  sealed trait FieldOperator {
    def string: String
  }

  sealed trait FieldOperatorTyped[T, +F <: TypedFieldFilter[T]] extends FieldOperator {
    def string: String

    def createFilter(field: String, value: T): F
  }

  sealed trait StringFieldOperator[+F <: StringFilter] extends FieldOperatorTyped[String, F]

  object EqualOperator extends StringFieldOperator[StringEqual] {
    override val string: String = "=="

    override def createFilter(field: String, value: String): StringEqual = StringEqual(field, value)
  }

  object NotEqualOperator extends StringFieldOperator[StringNotEqual] {
    override val string: String = "!="

    override def createFilter(field: String, value: String): StringNotEqual = StringNotEqual(field, value)
  }

  object ContainsOperator extends StringFieldOperator[StringContains] {
    override val string: String = "~"

    override def createFilter(field: String, value: String): StringContains = StringContains(field, value)
  }

  object NotContainsOperator extends StringFieldOperator[StringNotContains] {
    override val string: String = "!~"

    override def createFilter(field: String, value: String): StringNotContains = StringNotContains(field, value)
  }

  object MatchesOperator extends StringFieldOperator[StringMatches] {
    override val string: String = "=~"

    override def createFilter(field: String, value: String): StringMatches = StringMatches(field, value)
  }

  object NotMatchesOperator extends StringFieldOperator[StringNotMatches] {
    override val string: String = "!=~"

    override def createFilter(field: String, value: String): StringNotMatches = StringNotMatches(field, value)
  }

  sealed trait NumberFieldOperator[+F <: NumberFilter] extends FieldOperatorTyped[Double, F]

  object LessThanOperator extends NumberFieldOperator[LessThanFilter] {
    override def string: String = "<"

    override def createFilter(field: String, value: Double): LessThanFilter = LessThanFilter(field, value)
  }

  object LessOrEqualOperator extends NumberFieldOperator[LessOrEqualFilter] {
    override def string: String = "<="

    override def createFilter(field: String, value: Double): LessOrEqualFilter = LessOrEqualFilter(field, value)
  }

  object GreaterThanOperator extends NumberFieldOperator[GreaterThanFilter] {
    override def string: String = ">"

    override def createFilter(field: String, value: Double): GreaterThanFilter = GreaterThanFilter(field, value)
  }

  object GreaterOrEqualOperator extends NumberFieldOperator[GreaterOrEqualFilter] {
    override def string: String = ">="

    override def createFilter(field: String, value: Double): GreaterOrEqualFilter = GreaterOrEqualFilter(field, value)
  }

  trait TypedFieldFilter[T] extends FieldFilterExpression {
    def expectedFieldType: LunType

    def unpackValue(lunValue: LunValue): Either[Snag, T]

    def fieldTest(value: T): Boolean

    override def evaluate(record: LunValue.RecordValue): Either[Snag, PrimitiveValue.BoolValue] = {
      record.values.get(field) match {
        case Some(lunValue) =>
          for {
            castToType <- lunValue.castTo(expectedFieldType)
            unpackedValue <- unpackValue(castToType)
            result = fieldTest(unpackedValue)
          } yield PrimitiveValue.BoolValue(result)
        case None =>
          Left(Snag(s"Filter depends on field `$field`, but record ${record.id} does not have that field."))
      }
    }

    override def cleaned: TypedFieldFilter[T] = this
  }

  trait StringFilter extends TypedFieldFilter[String] {
    override def expectedFieldType: LunType.StringType.type = LunType.StringType

    override def unpackValue(lunValue: LunValue): Either[Snag, String] = lunValue.asString

    override def fieldTest(string: String): Boolean

    override def cleaned: StringFilter = this
  }

  case class StringEqual(field: String, value: String) extends StringFilter {
    override def fieldTest(string: String): Boolean = string == value
  }

  case class StringNotEqual(field: String, value: String) extends StringFilter {
    override def fieldTest(string: String): Boolean = string != value
  }

  case class StringContains(field: String, regexString: String) extends StringFilter {
    val regex: Regex = (".*" + regexString + ".*").r

    override def fieldTest(string: String): Boolean = regex.matches(string)
  }

  case class StringNotContains(field: String, regexString: String) extends StringFilter {
    val regex: Regex = (".*" + regexString + ".*").r

    override def fieldTest(string: String): Boolean = !regex.matches(string)
  }

  case class StringMatches(field: String, regexString: String) extends StringFilter {
    val regex: Regex = regexString.r

    override def fieldTest(string: String): Boolean = regex.matches(string)
  }

  case class StringNotMatches(field: String, regexString: String) extends StringFilter {
    val regex: Regex = regexString.r

    override def fieldTest(string: String): Boolean = !regex.matches(string)
  }

  trait NumberFilter extends TypedFieldFilter[Double] {
    override def expectedFieldType: LunType.FloatType.type = LunType.FloatType

    override def unpackValue(lunValue: LunValue): Either[Snag, Double] = lunValue.asDouble

    override def fieldTest(number: Double): Boolean

    override def cleaned: NumberFilter = this

  }

  case class LessThanFilter(field: String, value: Double) extends NumberFilter {
    override def fieldTest(number: Double): Boolean = number < value
  }

  case class LessOrEqualFilter(field: String, value: Double) extends NumberFilter {
    override def fieldTest(number: Double): Boolean = number <= value
  }

  case class GreaterThanFilter(field: String, value: Double) extends NumberFilter {
    override def fieldTest(number: Double): Boolean = number > value
  }

  case class GreaterOrEqualFilter(field: String, value: Double) extends NumberFilter {
    override def fieldTest(number: Double): Boolean = number >= value
  }

}
