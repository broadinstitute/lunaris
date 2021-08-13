package lunaris.selene

object Mion {
  sealed trait Expression {
    def to_code(): String
    def with_parens(): ExpressionWithParens = ExpressionWithParens(this)
    def member(identifier: Identifier): MemberExpression = MemberExpression(this, identifier)
    def call(arguments: Seq[Assignment]): CallExpression = CallExpression(this, arguments)
  }

  case class Identifier(name: String) extends Expression {
    override def to_code(): String = name
    def assign(expression: Expression): Assignment = Assignment(this, expression)
    def iter(expression: Expression): Iteration = Iteration(this, expression)
  }

  sealed trait Value extends Expression

  case class IntValue(int: Long) extends Value {
    override def to_code(): String = int.toString
  }

  case class FloatValue(float: Double) extends Value {
    override def to_code(): String = float.toString
  }

  case class StringValue(string: String) extends Value {
    val qq = "\""
    override def to_code(): String = s"$qq$string$qq"
  }

  case class BinOp(symbol: String)

  object BinOps {
    val plus: BinOp = BinOp("+")
  }

  case class BinaryExpression(lhs: Expression, op: BinOp, rhs: Expression)
    extends Expression {
    override def to_code(): String = s"${lhs.to_code()} ${op.symbol} ${rhs.to_code()}"
  }

  case class ExpressionWithParens(expression: Expression) extends Expression {
    override def to_code(): String = s"(${expression.to_code()})"
  }

  case class MemberExpression(expression: Expression, identifier: Identifier) extends Expression {
    override def to_code(): String = s"${expression.to_code()}.${identifier.to_code()}"
  }

  case class Assignment(identifier: Identifier, expression: Expression) extends Expression {
    override def to_code(): String = s"${identifier.to_code()} = ${expression.to_code()}"
  }

  case class CallExpression(callee: Expression, arguments: Seq[Assignment]) extends Expression {
    override def to_code(): String = {
      val arg_string = arguments.map(_.to_code()).mkString(", ")
      s"${callee.to_code()}($arg_string)"
    }
  }

  case class Iteration(identifier: Identifier, expression: Expression) extends Expression {
    override def to_code(): String = s"${identifier.to_code()} <- ${expression.to_code()}"
    def scatter(expression: Expression): Scatter = Scatter(this, expression)
  }

  case class Scatter(iteration: Iteration, expression: Expression) extends Expression {
    override def to_code(): String = s"(${iteration.to_code()}) ${expression.to_code()}"
  }

  case class Block(expressions: Seq[Expression]) extends Expression {
    override def to_code(): String = {
      expressions.map(expr => "  " + expr.to_code()).mkString("{\n", ";\n", ";\n}")
    }
  }

  case class Script(expressions: Seq[Expression]) {
    def to_code(): String = {
      expressions.map(_.to_code()).map(_ + ";\n").mkString("")
    }
  }

  def id(name: String): Identifier = Identifier(name)
  def str(string: String): StringValue = StringValue(string)
  def int(int: Long): IntValue = IntValue(int)
  def float(float: Double): FloatValue = FloatValue(float)
  def bin(lhs: Expression, op: BinOp, rhs: Expression): BinaryExpression = BinaryExpression(lhs, op, rhs)
}
