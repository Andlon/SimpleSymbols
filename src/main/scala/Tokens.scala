package simplesymbols
package tokens

import simplesymbols.expressions._

class InvalidToken(message: String) extends Exception(message) {}

sealed abstract class Token
case class NumberToken(number: Number) extends Token
case class VariableToken(name: String) extends Token

case class AdditionOperatorToken() extends Token with BinaryOperator {
  override val precedence = 2
  override def express(left: Expression, right: Expression) = new Sum(left, right)
  override val associativity: Associativity = Associative()
}

case class SubtractionOperatorToken() extends Token with BinaryOperator {
  override val precedence = 2
  override def express(left: Expression, right: Expression) = new Sum(left, new Product(new Constant(-1), right))
  override val associativity = LeftAssociative()
}

case class MultiplicationOperatorToken() extends Token with BinaryOperator {
  override val precedence = 3
  override def express(left: Expression, right: Expression) = new Product(left, right)
  override val associativity = Associative()
}

case class DivisionOperatorToken() extends Token with BinaryOperator {
  override val precedence = 3
  override def express(left: Expression, right: Expression) = new Constant(0) // TODO: Add division expression
  override val associativity = LeftAssociative()
}

case class ExponentiationOperatorToken() extends Token with BinaryOperator {
  override val precedence = 4
  override def express(left: Expression, right: Expression) = new Constant(0) // TODO: Add exponentiation expression
  override val associativity = RightAssociative()
}

object Tokens {
  lazy val plus = AdditionOperatorToken()
  lazy val minus = SubtractionOperatorToken()
  lazy val times = MultiplicationOperatorToken()
  lazy val divide = DivisionOperatorToken()
  lazy val power = ExponentiationOperatorToken()

  private lazy val variablePattern = """[A-Za-z][A-Za-z0-9_]*""".r
  private lazy val numberPattern = """[0-9.]*[0-9]""".r
  private lazy val operators: Seq[String] = Seq("+", "-", "*", "/", "^")

  def isValidVariableName(tokenStr: String) = variablePattern.pattern.matcher(tokenStr).matches
  def isValidNumber(tokenStr: String) = numberPattern.pattern.matcher(tokenStr).matches
  def isValidOperator(tokenStr: String) = operators contains tokenStr

  def operatorFromString(str: String): Token = str match {
    case "+" => plus
    case "-" => minus
    case "*" => times
    case "/" => divide
    case "^" => power
    case s => throw new InvalidToken("'" + s + "' does not correspond to a valid operator token.")
  }
}