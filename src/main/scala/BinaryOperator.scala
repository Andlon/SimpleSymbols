package simplesymbols

import simplesymbols.expressions.Expression

sealed abstract class Associativity
case class LeftAssociative() extends Associativity
case class RightAssociative() extends Associativity
case class Associative() extends Associativity
case class NonAssociative() extends Associativity

trait BinaryOperator {
  val precedence: Int
  val associativity: Associativity
  def express(left: Expression, right: Expression): Expression
}
