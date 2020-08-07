package lunaris.recipes.parsing

import fastparse.NoWhitespace._
import fastparse._
import lunaris.expressions.FieldFilterExpression
import lunaris.expressions.FieldFilterExpression.{EqualOperator, FieldOperator, MatchesOperator, NotEqualOperator, NotMatchesOperator}

object RecordExpressionParser {
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

  def fieldTest[_ : P]: P[FieldFilterExpression] =
    P(fieldName ~ fieldTestOperator ~ fieldTestValue).map {
      case (field, operator, value) => operator.createFilter(field, value)
    }

}
