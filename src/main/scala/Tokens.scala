package simplesymbols
package tokens

import simplesymbols.expressions._

class InvalidToken(message: String) extends Exception(message) {}

sealed abstract class Token
case class NumberToken(number: Number) extends Token with ExpressionValue {
  override lazy val expression = new Constant(number.doubleValue())
}

case class VariableToken(name: String) extends Token with ExpressionValue {
  override lazy val expression = new Variable(name)
}

case object AdditionOperatorToken extends Token with BinaryOperator {
  override val associativity: Associativity = LeftAssociative()
  override val precedence = 2
  override def express(left: Option[Expression], right: Option[Expression]) = (left, right) match {
    case (Some(a: Expression), Some(b: Expression)) => new Sum(a, b)
    case (None, Some(b: Expression)) => b
    case _ => throw new InvalidToken("Add operation is missing valid right-side expression.")
  }
}

case object SubtractionOperatorToken extends Token with BinaryOperator {
  override val associativity = LeftAssociative()
  override val precedence = 2
  override def express(left: Option[Expression], right: Option[Expression]) = (left, right) match {
    case (Some(a: Expression), Some(b: Expression)) => new Sum(a, new Product(new Constant(-1), b))
    case (None, Some(b: Expression)) => new Product(new Constant(-1), b)
    case _ => throw new InvalidToken("Subtract operation is missing valid right-side expression.")
  }
}

case object MultiplicationOperatorToken extends Token with BinaryOperator {
  override val associativity = LeftAssociative()
  override val precedence = 3
  override def express(left: Option[Expression], right: Option[Expression]) = (left, right) match {
    case (Some(a: Expression), Some(b: Expression)) => new Product(a, b)
    case _ => throw new InvalidToken("Multiply operation must have valid left- and right-side expressions.")
  }
}

case object DivisionOperatorToken extends Token with BinaryOperator {
  override val associativity = LeftAssociative()
  override val precedence = 3
  override def express(left: Option[Expression], right: Option[Expression]) = (left, right) match {
    case _ => throw new NotImplementedError("Division is not yet implemented.")
  }
}

case object ExponentiationOperatorToken extends Token with BinaryOperator {
  override val associativity = RightAssociative()
  override val precedence = 4
  override def express(left: Option[Expression], right: Option[Expression]) = (left, right) match {
    case _ => throw new NotImplementedError("Exponentiation is not yet implemented.")
  }
}

case object DefinitionToken extends Token
case object EqualityToken extends Token

object Tokens {
  val plus = AdditionOperatorToken
  val minus = SubtractionOperatorToken
  val multiplication = MultiplicationOperatorToken
  val division = DivisionOperatorToken
  val exponentiation = ExponentiationOperatorToken
  val definition = DefinitionToken
  val equality = EqualityToken

  def number(num: Number) = NumberToken(num)
  def variable(name: String) = VariableToken(name)

  private lazy val variablePattern = """[A-Za-z][A-Za-z0-9_]*""".r
  private lazy val numberPattern = """[0-9.]*[0-9]""".r
  lazy val operatorStrings: Seq[String] = Seq("+", "-", "*", "/", "^", ":=", "=")

  def isValidVariableName(tokenStr: String) = variablePattern.pattern.matcher(tokenStr).matches
  def isValidNumber(tokenStr: String) = numberPattern.pattern.matcher(tokenStr).matches
  def isValidOperator(tokenStr: String) = operatorStrings contains tokenStr

  def operatorFromString(str: String): Token = str match {
    case "+" => plus
    case "-" => minus
    case "*" => multiplication
    case "/" => division
    case "^" => exponentiation
    case ":=" => definition
    case "=" => equality
    case s => throw new InvalidToken("'" + s + "' does not correspond to a valid operator token.")
  }
}