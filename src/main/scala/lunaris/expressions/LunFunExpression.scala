package lunaris.expressions
import lunaris.expressions
import lunaris.recipes.values.{LunType, LunValue}
import org.broadinstitute.yootilz.core.snag.Snag

case class LunFunExpression(funName: String, args: Seq[LunExpression], returnType: LunType)
  extends LunExpression {
  override def fields: Set[String] = args.toSet[expressions.LunExpression].flatMap(_.fields)

  override def evaluate(record: LunValue.RecordValue): Either[Snag, LunValue] = ???

  override def cleaned: LunExpression = ???
}
