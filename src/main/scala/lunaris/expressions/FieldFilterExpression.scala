package lunaris.expressions

import lunaris.recipes.values.{LunType, LunValue}
import lunaris.recipes.values.LunValue.PrimitiveValue
import org.broadinstitute.yootilz.core.snag.Snag
import scala.util.matching.Regex


trait FieldFilterExpression extends BooleanRecordExpression {
  def field: String

  override def fields: Set[String] = Set(field)

  override def cleaned: FieldFilterExpression
}

object FieldFilterExpression {

  sealed trait FieldOperator {
    def string: String

    def createFilter(field: String, value: String): StringFilter
  }

  sealed trait FieldOperatorTyped[F <: StringFilter] extends FieldOperator {
    def string: String

    def createFilter(field: String, value: String): F
  }

  object EqualOperator extends FieldOperatorTyped[StringEqual] {
    override val string: String = "=="

    override def createFilter(field: String, value: String): StringEqual = StringEqual(field, value)
  }

  object NotEqualOperator extends FieldOperatorTyped[StringNotEqual] {
    override val string: String = "!="

    override def createFilter(field: String, value: String): StringNotEqual = StringNotEqual(field, value)
  }

  object MatchesOperator extends FieldOperatorTyped[StringMatches] {
    override val string: String = "=~"

    override def createFilter(field: String, value: String): StringMatches = StringMatches(field, value)
  }

  object NotMatchesOperator extends FieldOperatorTyped[StringNotMatches] {
    override val string: String = "!=~"

    override def createFilter(field: String, value: String): StringNotMatches = StringNotMatches(field, value)
  }

  trait StringFilter extends FieldFilterExpression {
    def stringTest(string: String): Boolean

    override def evaluate(record: LunValue.RecordValue): Either[Snag, PrimitiveValue.BoolValue] = {
      record.values.get(field) match {
        case Some(lunValue) =>
          for {
            stringCast <- lunValue.castTo(LunType.StringType)
            string <- stringCast.asString
            result = stringTest(string)
          } yield PrimitiveValue.BoolValue(result)
        case None => Right(PrimitiveValue.BoolValue(false))
      }
    }

    override def cleaned: StringFilter = this
  }

  case class StringEqual(field: String, value: String) extends StringFilter {
    override def stringTest(string: String): Boolean = string == value
  }

  case class StringNotEqual(field: String, value: String) extends StringFilter {
    override def stringTest(string: String): Boolean = string != value
  }

  case class StringMatches(field: String, regexString: String) extends StringFilter {
    val regex: Regex = regexString.r

    override def stringTest(string: String): Boolean = regex.matches(string)
  }

  case class StringNotMatches(field: String, regexString: String) extends StringFilter {
    val regex: Regex = regexString.r

    override def stringTest(string: String): Boolean = !regex.matches(string)
  }


}
