package simplesymbols
package expressions

sealed abstract class Expression {
  type Environment = Map[Variable, Expression]

  def eval(env: Environment): Expression
  def simplified: Expression
}

case class Constant(val constant: Double) extends Expression {
  override def eval(env: Environment) = this
  override def simplified = this
}

case class Sum(val left: Expression, val right: Expression) extends Expression {
  override def eval(env: Environment) = Sum(left.eval(env), right.eval(env))
  override def simplified = (left.simplified, right.simplified) match {
    case (Constant(a), Constant(b)) => Constant(a + b)
    case (a, b) => Sum(a, b)
  }
}

case class Product(val left: Expression, val right: Expression) extends Expression {
  override def eval(env: Environment) = Product(left.eval(env), right.eval(env))

  override def simplified: Expression = (left.simplified, right.simplified) match {
    case (Constant(a), Constant(b)) => Constant(a * b)
    case (a, b) => Product(a, b)
  }
}

case class Variable(val name: String) extends Expression {
  override def eval(env: Environment) = env.getOrElse(this, this)
  override def simplified: Expression = this
  override def hashCode = name.hashCode
}