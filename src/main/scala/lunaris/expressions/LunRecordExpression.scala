package lunaris.expressions

import lunaris.recipes.values.LunValue
import org.broadinstitute.yootilz.core.snag.Snag

trait LunRecordExpression[+V <: LunValue] {
  def fields: Set[String]
  def evaluate(record: LunValue.RecordValue): Either[Snag, V]
}

object LunRecordExpression {
  trait OperatorChainExpression[+V <: LunValue] extends LunRecordExpression[V] {
    def terms: Seq[LunRecordExpression[V]]
    def fields: Set[String] = terms.toSet[LunRecordExpression[V]].flatMap(_.fields)
  }

  trait PrimitiveOperatorChain[V, L <: LunValue.PrimitiveValue.LunTypedPrimitiveValue[V]]
    extends OperatorChainExpression[L] {
    def zeroValue: V
    def fold(value1: V, value2: V): V
    def isTerminal(value: V): Boolean
    def liftValue(value: V): L

    override def evaluate(record: LunValue.RecordValue): Either[Snag, L] = {
      var snagOpt: Option[Snag] = None
      var result = zeroValue
      val termsIter = terms.iterator
      while(snagOpt.isEmpty && termsIter.hasNext && !isTerminal(result)) {
        val term = termsIter.next()
        term.evaluate(record) match {
          case Left(snag) => snagOpt = Some(snag)
          case Right(lunValue) => result = fold(result, lunValue.value)
        }
      }
      snagOpt match {
        case Some(snag) => Left(snag)
        case None => Right(liftValue(result))
      }
    }
  }
}

