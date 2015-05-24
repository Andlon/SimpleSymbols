package simplesymbols
package expressions

abstract class Expression {
  /**
   * Evaluates the expression given an environment. The environment defines the values of variables that appear
   * in the expression.
   * @param env The environment whose values to use for the evaluation.
   * @return A real value corresponding to the value of the expression.
   */
  def eval(env: Environment): Double

  /**
   * Returns a new expression with any variables in environment replaced with constants with values defined by those
   * in environment.
   * @param env The environment from which to retrieve values of variables.
   * @return A new expression where variables from env are replaced with constants.
   */
  def replace(env: Environment): Expression
}

class Constant(val constant: Double) extends Expression {
  override def eval(env: Environment) = constant
  override def replace(env: Environment) = this
}

class Sum(val left: Expression, val right: Expression) extends Expression {
  override def eval(env: Environment) = left.eval(env) + right.eval(env)
  override def replace(env: Environment) = new Sum(left replace env, right replace env)
}

class Product(val left: Expression, val right: Expression) extends Expression {
  override def eval(env: Environment) = left.eval(env) * right.eval(env)
  override def replace(env: Environment) = new Product(left replace env, right replace env)
}

class Variable(val name: String) extends Expression {
  override def eval(env: Environment) = env.get(name)
  override def replace(env: Environment) = if (env contains name) new Constant(env get name) else this
}
