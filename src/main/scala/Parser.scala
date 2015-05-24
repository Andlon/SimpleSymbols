package simplesymbols

import simplesymbols.expressions._

object Parser {

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
