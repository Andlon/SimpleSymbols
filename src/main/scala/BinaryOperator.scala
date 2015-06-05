package simplesymbols

import simplesymbols.expressions.Expression

sealed abstract class Associativity
case class LeftAssociative() extends Associativity
case class RightAssociative() extends Associativity

trait BinaryOperator {
  val precedence: Int
  val associativity: Associativity
  def express(left: Option[Expression], right: Option[Expression]): Expression

  def leftAssociative = associativity == LeftAssociative()
  def rightAssociative = associativity == RightAssociative()
}
