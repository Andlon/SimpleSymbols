package simplesymbols
package functions

abstract class Function {
  def eval(env: Environment): Double
}

class Constant(val constant: Double) extends Function {
  override def eval(env: Environment) = constant
}

class Sum(val left: Function, val right: Function) extends Function {
  override def eval(env: Environment) = left.eval(env) + right.eval(env)
}

class Product(val left: Function, val right: Function) extends Function {
  override def eval(env: Environment) = left.eval(env) * right.eval(env)
}

class Variable(val name: String) extends Function {
  override def eval(env: Environment) = env.get(name)
}
