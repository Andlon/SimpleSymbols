package simplesymbols

import simplesymbols.expressions._
import simplesymbols.tokens._

import scala.annotation.tailrec

object Parser {
  def isVariableName(tokenStr: String) = Tokens.isValidVariableName(tokenStr)
  def isNumber(tokenStr: String) = Tokens.isValidNumber(tokenStr)
  def isOperator(tokenStr: String) = Tokens.isValidOperator(tokenStr)

  private def extractToken(tokenStr: String): Option[Token] = tokenStr match {
    case "" => None
    case s if Tokens.isValidOperator(tokenStr) => Option(Tokens.operatorFromString(tokenStr))
    case s if isVariableName(s) => Option(VariableToken(s))
    case s if isNumber(s) => Option(NumberToken(s.toDouble)) // Temporarily support only double and ignore error handling
    case _ => throw new UnsupportedOperationException("TODO: Throw custom exception")
  }

  def tokenize(statement: String): Seq[Token] = tokenizePartialStatement(statement, "", Seq())

  // A sorted list of operator strings, sorted by length in descending order.
  // This way, longer operators such as ":=" are matched before shorter operators with common characters (e.g. "=")
  private lazy val operators = Tokens.operatorStrings.sortWith(_.length > _.length)

  @tailrec
  private def tokenizePartialStatement(statement: String, currentTokenStr: String, currentSeq: Seq[Token]) : Seq[Token] = {
    lazy val token = extractToken(currentTokenStr)
    lazy val firstOp = operators.collectFirst({ case op if statement.startsWith(op) => op })
    statement match {
      case "" => currentSeq ++ token
      case s if s.head.isSpaceChar => tokenizePartialStatement(s.tail, currentTokenStr, currentSeq)
      case s if firstOp.isDefined =>
        tokenizePartialStatement(s drop firstOp.get.size, "", currentSeq ++ token ++ extractToken(firstOp.get))
      case s => tokenizePartialStatement(s.tail, currentTokenStr + s.head, currentSeq)
    }
  }

  def assemble(tokens: Seq[Token]): Expression =
    if (tokens.nonEmpty) shuntingYard(tokens, List(), List())
    else throw new UnsupportedOperationException("List of tokens must be non-empty")

  @tailrec
  private def shuntingYard(remaining: Seq[Token], stack: List[BinaryOperator], output: List[Expression]): Expression =
    if (remaining.nonEmpty) remaining.head match {
      case op: BinaryOperator if stack.nonEmpty => stack.head match {
        case op2: BinaryOperator if op.leftAssociative && op.precedence <= op2.precedence =>
          shuntingYard(remaining, stack.tail, addOperatorToStack(op2, output))
        case op2: BinaryOperator if op.rightAssociative && op.precedence > op2.precedence =>
          shuntingYard(remaining, stack.tail, addOperatorToStack(op2, output))
        case _ => shuntingYard(remaining.tail, op :: stack, output)
      }
      case op: BinaryOperator => shuntingYard(remaining.tail, op :: stack, output)
      case v: ExpressionValue => shuntingYard(remaining.tail, stack, v.expression :: output)
    }
    else if (stack.nonEmpty) shuntingYard(Seq(), stack.tail, addOperatorToStack(stack.head, output))
    else output.head

  private def addOperatorToStack(op: BinaryOperator, stack: List[Expression]): List[Expression] = stack.length match {
    case 0 => List(op.express(None, None))
    case 1 => List(op.express(None, Some(stack.head)))
    case _ => op.express(Some(stack.tail.head), Some(stack.head)) :: (stack drop 2)
  }

  def parse(expression: String): Expression = assemble(tokenize(expression))
}
