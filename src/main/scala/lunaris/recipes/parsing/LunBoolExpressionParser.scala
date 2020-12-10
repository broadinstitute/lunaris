package lunaris.recipes.parsing

import fastparse.ScalaWhitespace._
import fastparse._
import fastparse.{NoCut, P, Parsed}
import lunaris.expressions.LunBoolExpression
import lunaris.expressions.LunBoolExpression.{AndExpression, OrExpression}
import lunaris.expressions.FieldFilterExpression.{ContainsOperator, EqualOperator, GreaterOrEqualOperator, GreaterThanOperator, LessOrEqualOperator, LessThanOperator, MatchesOperator, NotContainsOperator, NotEqualOperator, NotMatchesOperator, NumberFieldOperator, NumberFilter, StringFieldOperator, StringFilter}
import lunaris.recipes.parsing.LunBasicParser.{fieldName, numberLiteral, stringLiteral}
import org.broadinstitute.yootilz.core.snag.Snag

object LunBoolExpressionParser {
  def equalOperator[_: P]: P[EqualOperator.type] = P(EqualOperator.string).!.map(_ => EqualOperator)

  def notEqualOperator[_: P]: P[NotEqualOperator.type] = P(NotEqualOperator.string).!.map(_ => NotEqualOperator)

  def containsOperator[_: P]: P[ContainsOperator.type] =
    P(ContainsOperator.string).!.map(_ => ContainsOperator)

  def notContainsOperator[_: P]: P[NotContainsOperator.type] =
    P(NotContainsOperator.string).!.map(_ => NotContainsOperator)

  def matchesOperator[_: P]: P[MatchesOperator.type] = P(MatchesOperator.string).!.map(_ => MatchesOperator)

  def notMatchesOperator[_: P]: P[NotMatchesOperator.type] =
    P(NotMatchesOperator.string).!.map(_ => NotMatchesOperator)

  def stringTestOperator[_: P]: P[StringFieldOperator[StringFilter]] =
    P(equalOperator | notEqualOperator | containsOperator | notContainsOperator | matchesOperator |
      notMatchesOperator)

  def stringFieldTest[_: P]: P[StringFilter] =
    P(NoCut(fieldName) ~ stringTestOperator ~ stringLiteral).map {
      case (field, operator, value) => operator.createFilter(field, value)
    }

  def lessThanOperator[_: P]: P[LessThanOperator.type] = P(LessThanOperator.string).!.map(_ => LessThanOperator)

  def lessOrEqualOperator[_: P]: P[LessOrEqualOperator.type] =
    P(LessOrEqualOperator.string).!.map(_ => LessOrEqualOperator)

  def greaterThanOperator[_: P]: P[GreaterThanOperator.type] =
    P(GreaterThanOperator.string).!.map(_ => GreaterThanOperator)

  def greaterOrEqualOperator[_: P]: P[GreaterOrEqualOperator.type] =
    P(GreaterOrEqualOperator.string).!.map(_ => GreaterOrEqualOperator)

  def numberTestOperator[_: P]: P[NumberFieldOperator[NumberFilter]] =
    P(lessOrEqualOperator | lessThanOperator | greaterOrEqualOperator | greaterThanOperator)

  def numberFieldTest[_: P]: P[NumberFilter] =
    P(NoCut(fieldName) ~ numberTestOperator ~ numberLiteral).map {
      case (field, operator, value) => operator.createFilter(field, value)
    }

  def parensExpression[_: P]: P[OrExpression] = P(P("(") ~/ orExpression ~ P(")"))

  def tightFilterExpression[_: P]: P[LunBoolExpression] =
    P(stringFieldTest | numberFieldTest | parensExpression)

  def andExpression[_: P]: P[AndExpression] =
    P(tightFilterExpression ~ (P("AND") ~ tightFilterExpression).rep).map {
      case (expression, expressions) => AndExpression(expression +: expressions)
    }

  def orExpression[_: P]: P[OrExpression] =
    P(andExpression ~ (P("OR") ~ andExpression).rep).map {
      case (expression, expressions) => OrExpression(expression +: expressions)
    }

  def entireFilter[_: P]: P[OrExpression] = orExpression ~ End

  def parse(string: String, emptyMeansAlwaysPass: Boolean = true): Either[Snag, LunBoolExpression] = {
    if (emptyMeansAlwaysPass && string.trim.isEmpty) {
      Right(LunBoolExpression.True)
    } else {
      fastparse.parse(string, LunBoolExpressionParser.entireFilter(_)) match {
        case failure: Parsed.Failure =>
          val trace = failure.trace()
          Left(Snag(trace.msg, trace.longMsg))
        case Parsed.Success(expression, _) =>
          Right(expression.cleaned)
      }
    }
  }
}
