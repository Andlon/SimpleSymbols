package simplesymbols

import simplesymbols.expressions._
import simplesymbols.tokens._

object Parser {
  def tokenize(statement: String): Seq[Token] = tokenizePartialStatement(statement, "")

  def precedenceOf(operator: Char) = operator match {
    case '+' | '-' => 2
    case '*' | '/' => 3
    case '^' => 4
    case _ => throw new UnsupportedOperationException("Unsupported operator " + operator + ".")
  }

  private def tokenizePartialStatement(statement: String, currentTokenStr: String) : Seq[Token] =
    statement match {
      case s if s.isEmpty && currentTokenStr.isEmpty => Seq()
      case s if s.isEmpty => extractToken(currentTokenStr)
      case s if "+-*/".contains(s.head) =>
        extractToken(currentTokenStr) ++
          Seq(LeftAssocOperatorToken(s.head, precedenceOf(s.head))) ++
          tokenizePartialStatement(s.tail, "")
      case s if "^".contains(s.head) =>
        extractToken(currentTokenStr) ++
          Seq(RightAssocOperatorToken(s.head, precedenceOf(s.head))) ++
          tokenizePartialStatement(s.tail, "")
      case s if s.head.isSpaceChar => tokenizePartialStatement(s.tail, currentTokenStr)
      case s => tokenizePartialStatement(s.tail, currentTokenStr + s.head)
    }

  private lazy val variablePattern = """[A-Za-z][A-Za-z0-9_]*""".r
  private lazy val numberPattern = """[0-9.]*[0-9]""".r

  def isValidVariableName(tokenStr: String) = variablePattern.pattern.matcher(tokenStr).matches
  def isValidNumber(tokenStr: String) = numberPattern.pattern.matcher(tokenStr).matches

  private def extractToken(tokenStr: String): Seq[Token] = tokenStr match {
    case "" => Seq()
    case s if isValidVariableName(s) => Seq(VariableToken(s))
    case s if isValidNumber(s) => Seq(NumberToken(s.toDouble)) // Temporarily support only double and ignore error handling
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
