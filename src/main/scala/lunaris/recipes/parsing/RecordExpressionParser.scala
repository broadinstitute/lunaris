package lunaris.recipes.parsing

import fastparse.NoWhitespace._
import fastparse._
import lunaris.expressions.BooleanRecordExpression.{AndExpression, OrExpression}
import lunaris.expressions.{BooleanRecordExpression, FieldFilterExpression}
import lunaris.expressions.FieldFilterExpression.{EqualOperator, FieldOperator, MatchesOperator, NotEqualOperator, NotMatchesOperator}
import org.broadinstitute.yootilz.core.snag.Snag

object RecordExpressionParser {

  object Parsers {

    def bareFieldNameStart[_: P]: P[Unit] = P(CharPred(Character.isJavaIdentifierStart))

    def bareFieldNamePart[_: P]: P[Unit] = P(CharPred(Character.isJavaIdentifierPart))

    def bareFieldName[_: P]: P[String] = P(bareFieldNameStart ~ bareFieldNamePart.rep).!

    def tickedFieldName[_: P]: P[String] = P(P("`") ~ CharsWhile(_ != '`').! ~ P("`"))

    def fieldName[_: P]: P[String] = P(bareFieldName | tickedFieldName)

    def equalOperator[_: P]: P[EqualOperator.type] = P(EqualOperator.string).!.map(_ => EqualOperator)

    def notEqualOperator[_: P]: P[NotEqualOperator.type] = P(NotEqualOperator.string).!.map(_ => NotEqualOperator)

    def matchesOperator[_: P]: P[MatchesOperator.type] = P(MatchesOperator.string).!.map(_ => MatchesOperator)

    def notMatchesOperator[_: P]: P[NotMatchesOperator.type] =
      P(NotMatchesOperator.string).!.map(_ => NotMatchesOperator)

    def fieldTestOperator[_: P]: P[FieldOperator] =
      P(equalOperator | notEqualOperator | matchesOperator | notMatchesOperator)

    def fieldTestValue[_: P]: P[String] = P(P("\"") ~ CharPred(_ != '"').rep.! ~ P("\""))

    def fieldTest[_: P]: P[FieldFilterExpression] =
      P(fieldName ~ fieldTestOperator ~ fieldTestValue).map {
        case (field, operator, value) => operator.createFilter(field, value)
      }

    def parensExpression[_: P]: P[OrExpression] = P(P("(") ~ orExpression ~ P(")"))

    def tightFilterExpression[_: P]: P[BooleanRecordExpression] = P(fieldTest | parensExpression)

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
    if(emptyMeansAlwaysPass && string.trim.isEmpty) {
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
