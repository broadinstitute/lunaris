package lunaris.expressions

import lunaris.recipes.values.LunValue
import lunaris.recipes.values.LunValue.PrimitiveValue.BoolValue

trait BooleanRecordExpression extends LunRecordExpression[BoolValue] {

}

object BooleanRecordExpression {

  case class AndExpression(terms: Seq[BooleanRecordExpression])
    extends BooleanRecordExpression with LunRecordExpression.PrimitiveOperatorChain[Boolean, BoolValue] {
    override def zeroValue: Boolean = true

    override def fold(value1: Boolean, value2: Boolean): Boolean = value1 && value2

    override def isTerminal(value: Boolean): Boolean = !value

    override def liftValue(value: Boolean): BoolValue = LunValue.PrimitiveValue.BoolValue(value)
  }

  case class OrExpression(terms: Seq[BooleanRecordExpression])
    extends BooleanRecordExpression with LunRecordExpression.PrimitiveOperatorChain[Boolean, BoolValue] {
    override def zeroValue: Boolean = false

    override def fold(value1: Boolean, value2: Boolean): Boolean = value1 || value2

    override def isTerminal(value: Boolean): Boolean = value

    override def liftValue(value: Boolean): BoolValue = LunValue.PrimitiveValue.BoolValue(value)
  }

}