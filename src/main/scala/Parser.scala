package simplesymbols

import simplesymbols.expressions._
import simplesymbols.tokens._

import scala.annotation.tailrec

object Parser {
  def tokenize(statement: String): Seq[Token] = tokenizePartialStatement(statement, "", Seq())

  @tailrec
  def tokenizePartialStatement(statement: String, currentTokenStr: String, currentSeq: Seq[Token]) : Seq[Token] = {
    lazy val token = extractToken(currentTokenStr)
    statement match {
      case s if s.isEmpty => currentSeq ++ token
      case s if isOperator(s.head.toString) =>
        tokenizePartialStatement(s.tail, "", currentSeq ++ token ++ extractToken(s.head.toString))
      case s if s.head.isSpaceChar => tokenizePartialStatement(s.tail, currentTokenStr, currentSeq)
      case s => tokenizePartialStatement(s.tail, currentTokenStr + s.head, currentSeq)
    }
  }

  private lazy val variablePattern = """[A-Za-z][A-Za-z0-9_]*""".r
  private lazy val numberPattern = """[0-9.]*[0-9]""".r

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

  def parse(expression: String): Expression =
    parseForSum(expression.filterNot(_.isSpaceChar).replace("-", "+-"))

  private def parseForSum(expression: String): Expression = {
    val (leftExpr, rightExpr) = splitBySymbol(expression, '+')
    lazy val leftFunc = parseForProduct(leftExpr)
    lazy val rightFunc = parseForSum(rightExpr)

    rightExpr match {
      case "" => leftFunc
      case _ => new Sum(leftFunc, rightFunc)
    }
  }

  private def parseForProduct(expression: String): Expression = {
    val (leftExpr, rightExpr) = splitBySymbol(expression, '*')

    lazy val leftFunc = parseUnary(leftExpr)
    lazy val rightFunc = parseForProduct(rightExpr)

    (leftExpr, rightExpr) match {
      case ("", "") => new Constant(0.0)
      case ("", _) => rightFunc
      case (_, "") => leftFunc
      case (_, _) => new Product(leftFunc, rightFunc)
    }
  }

  private def parseUnary(expression: String): Expression =
    try { new Constant(expression.toDouble) }
    catch { case _: NumberFormatException => {
      expression.head match {
        case '-' => new Product(new Constant(-1.0), new Variable(expression.tail))
        case _ => new Variable(expression)
      }
    }
    }

  private def splitBySymbol(str: String, symbol: Char) = str.indexOf(symbol) match {
    case -1 => (str, "")
    case n => (str take n, str drop n + 1)
  }
}
