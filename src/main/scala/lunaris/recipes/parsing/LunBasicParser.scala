package lunaris.recipes.parsing

import fastparse.ScalaWhitespace._
import fastparse._
import fastparse.{CharIn, CharPred, CharsWhile, CharsWhileIn, P}

object LunBasicParser {
  def bareFieldNameStart[_: P]: P[Unit] = P(CharPred(Character.isJavaIdentifierStart))

  def bareFieldNamePart[_: P]: P[Unit] = P(CharPred(Character.isJavaIdentifierPart))

  def bareFieldName[_: P]: P[String] = P(bareFieldNameStart ~/ bareFieldNamePart.rep).!

  def tickedFieldName[_: P]: P[String] = P(P("`") ~/ CharsWhile(_ != '`').! ~ P("`"))

  def fieldName[_: P]: P[String] = P(bareFieldName | tickedFieldName)

  def stringLiteral[_: P]: P[String] = P(P("\"") ~/ CharPred(_ != '"').rep.! ~ P("\""))

  // Floating point parsing adapted from FastParse JSON example
  def digits[_: P]: P[Unit] = P(CharsWhileIn("0-9"))

  def exponent[_: P]: P[Unit] = P(CharIn("eE") ~ CharIn("+\\-").? ~ digits)

  def fractional[_: P]: P[Unit] = P("." ~ digits)

  def integral[_: P]: P[Unit] = P("0" | CharIn("1-9") ~ digits.?)

  def numberLiteral[_: P]: P[Double] =
    P(CharIn("+\\-").? ~ integral ~ fractional.? ~ exponent.?).!.map(_.toDouble)

}

