package lunaris.expressions

import lunaris.expressions.LunExpression.LunExpressionTyped
import lunaris.recipes.values.{LunType, LunValue}
import lunaris.recipes.values.LunValue.PrimitiveValue.BoolValue
import org.broadinstitute.yootilz.core.snag.Snag

trait LunBoolExpression extends LunExpressionTyped[BoolValue] {
  override def cleaned: LunBoolExpression

  override def returnType: LunType.BoolType.type = LunType.BoolType
}

object LunBoolExpression {

  object True extends LunBoolExpression {
    override def fields: Set[String] = Set.empty

    override def evaluate(record: LunValue.RecordValue): Either[Snag, BoolValue] = Right(BoolValue(true))

    override def cleaned: True.type = this
  }

  object False extends LunBoolExpression {
    override def fields: Set[String] = Set.empty

    override def evaluate(record: LunValue.RecordValue): Either[Snag, BoolValue] = Right(BoolValue(false))

    override def cleaned: False.type = this
  }

  case class AndExpression(terms: Seq[LunBoolExpression])
    extends LunBoolExpression with LunExpression.PrimitiveOperatorChain[Boolean, BoolValue] {
    override def zeroValue: Boolean = true

    override def fold(value1: Boolean, value2: Boolean): Boolean = value1 && value2

    override def isTerminal(value: Boolean): Boolean = !value

    override def liftValue(value: Boolean): BoolValue = LunValue.PrimitiveValue.BoolValue(value)

    override def cleaned: LunBoolExpression = {
      val termsCleaned = terms.map(_.cleaned).flatMap {
        case AndExpression(subTerms) => subTerms
        case True => Seq()
        case term => Seq(term)
      }
      if (termsCleaned.isEmpty) {
        True
      } else if (termsCleaned.size == 1) {
        termsCleaned.head
      } else if (termsCleaned.contains(False)) {
        False
      } else {
        AndExpression(termsCleaned)
      }
    }
  }

  case class OrExpression(terms: Seq[LunBoolExpression])
    extends LunBoolExpression with LunExpression.PrimitiveOperatorChain[Boolean, BoolValue] {
    override def zeroValue: Boolean = false

    override def fold(value1: Boolean, value2: Boolean): Boolean = value1 || value2

    override def isTerminal(value: Boolean): Boolean = value

    override def liftValue(value: Boolean): BoolValue = LunValue.PrimitiveValue.BoolValue(value)

    override def cleaned: LunBoolExpression = {
      val termsCleaned = terms.map(_.cleaned).flatMap {
        case OrExpression(subTerms) => subTerms
        case False => Seq()
        case term => Seq(term)
      }
      if (termsCleaned.isEmpty) {
        False
      } else if (termsCleaned.size == 1) {
        termsCleaned.head
      } else if (termsCleaned.contains(True)) {
        True
      } else {
        OrExpression(termsCleaned)
      }
    }
  }

}