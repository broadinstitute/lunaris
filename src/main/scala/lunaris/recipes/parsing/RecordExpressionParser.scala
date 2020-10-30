package lunaris.recipes.parsing

import fastparse.ScalaWhitespace._
import fastparse._
import lunaris.expressions.BooleanRecordExpression
import lunaris.expressions.BooleanRecordExpression.{AndExpression, OrExpression}
import lunaris.expressions.FieldFilterExpression._
import org.broadinstitute.yootilz.core.snag.Snag

object RecordExpressionParser {

  object Parsers {

    def bareFieldNameStart[_: P]: P[Unit] = P(CharPred(Character.isJavaIdentifierStart))

    def bareFieldNamePart[_: P]: P[Unit] = P(CharPred(Character.isJavaIdentifierPart))

    def bareFieldName[_: P]: P[String] = P(bareFieldNameStart ~/ bareFieldNamePart.rep).!

    def tickedFieldName[_: P]: P[String] = P(P("`") ~/ CharsWhile(_ != '`').! ~ P("`"))

    def fieldName[_: P]: P[String] = P(bareFieldName | tickedFieldName)

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

    def stringLiteral[_: P]: P[String] = P(P("\"") ~/ CharPred(_ != '"').rep.! ~ P("\""))

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

    // Floating point parsing adapted from FastParse JSON example
    def digits[_: P]: P[Unit] = P(CharsWhileIn("0-9"))

    def exponent[_: P]: P[Unit] = P(CharIn("eE") ~ CharIn("+\\-").? ~ digits)

    def fractional[_: P]: P[Unit] = P("." ~ digits)

    def integral[_: P]: P[Unit] = P("0" | CharIn("1-9") ~ digits.?)

    def numberLiteral[_: P]: P[Double] =
      P(CharIn("+\\-").? ~ integral ~ fractional.? ~ exponent.?).!.map(_.toDouble)

    def numberFieldTest[_: P]: P[NumberFilter] =
      P(NoCut(fieldName) ~ numberTestOperator ~ numberLiteral).map {
        case (field, operator, value) => operator.createFilter(field, value)
      }

    def parensExpression[_: P]: P[OrExpression] = P(P("(") ~/ orExpression ~ P(")"))

    def tightFilterExpression[_: P]: P[BooleanRecordExpression] =
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
  }

  def parse(string: String, emptyMeansAlwaysPass: Boolean = true): Either[Snag, BooleanRecordExpression] = {
    if (emptyMeansAlwaysPass && string.trim.isEmpty) {
      Right(BooleanRecordExpression.True)
    } else {
      fastparse.parse(string, Parsers.entireFilter(_)) match {
        case failure: Parsed.Failure =>
          val trace = failure.trace()
          Left(Snag(trace.msg, trace.longMsg))
        case Parsed.Success(expression, _) =>
          Right(expression.cleaned)
      }
    }
  }

}
