package simplesymbols
package tokens

sealed abstract class Token

case class LeftAssocOperatorToken(symbol: Char, precedence: Int) extends Token
case class RightAssocOperatorToken(symbol: Char, precedence: Int) extends Token
case class NumberToken(number: Number) extends Token
case class VariableToken(name: String) extends Token